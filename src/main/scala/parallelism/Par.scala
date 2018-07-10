package fpscala.parallelism

import java.util.concurrent._
object Par {
  type Par[A] = ExecutorService => Future[A]
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
    (es: ExecutorService) =>
      {
        val af = a(es)
        val bf = b(es)
        UnitFuture(f(af.get, bf.get))
      }
  }

  def fork[A](a: => Par[A]): Par[A] = { ex =>
    ex.submit(new Callable[A] {
      def call = a(ex).get
    })
  }

  def map2WithTimeout[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C) = {
    (es: ExecutorService) =>
      {
        Map2Future(a(es), b(es), f)
      }
  }

  def run[A](es: ExecutorService)(a: Par[A]): Future[A] = a(es)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit): A = get

    def isCancelled = false

    def cancel(eventIfRuning: Boolean): Boolean = false
  }

}

case class Map2Future[A, B, C](a: Future[A], b: Future[B], f: (A, B) => C)
    extends Future[C] {

  @volatile
  var cache: Option[C] = None

  def isDone = cache.isDefined

  def isCancelled = a.isCancelled || b.isCancelled

  def cancel(eventIfRuning: Boolean) =
    a.cancel(eventIfRuning) || b.cancel(eventIfRuning)

  def get: C = compute(Long.MaxValue)
  def get(timeout: Long, units: TimeUnit): C =
    compute(TimeUnit.NANOSECONDS.convert(timeout, units))

  private def compute(timeoutInNano: Long): C = {
    cache match {
      case Some(r) => r
      case _ => {
        val start = System.nanoTime
        val ar = a.get(timeoutInNano, TimeUnit.NANOSECONDS)
        val end = System.nanoTime
        val br = b.get(timeoutInNano - (end - start), TimeUnit.NANOSECONDS)
        val cr = f(ar, br)
        cache = Some(cr)
        cr
      }
    }
  }
}
