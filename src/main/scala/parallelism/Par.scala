package fpscala.parallelism

import java.util.concurrent.{Future => JFuture}
import java.util.concurrent.ExecutorService
import java.util.concurrent.TimeUnit
import java.util.concurrent.Callable
object Par {
  type Par[A] = ExecutorService => JFuture[A]
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

  def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

  def lazyUnit[A](a: A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = { (a: A) =>
    lazyUnit(f(a))
  }

  def equals[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](unit(List()))((a, b) => map2(a, b)(_ :: _))

  def map[A, B](a: Par[A])(f: A => B): Par[B] =
    map2(a, unit(()))((a, _) => f(a))

  def parMap[A, B](a: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = a.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](a: List[A])(f: A => Boolean): Par[List[A]] = {
    val fbs: List[Par[List[A]]] =
      a.map(asyncF((a: A) => if (f(a)) List(a) else List()))
    map(sequence(fbs))(_.flatten)
  }

  def map2WithTimeout[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C) = {
    (es: ExecutorService) =>
      {
        Map2Future(a(es), b(es), f)
      }
  }

  def run[A](es: ExecutorService)(a: Par[A]): JFuture[A] = a(es)

  private case class UnitFuture[A](get: A) extends JFuture[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit): A = get

    def isCancelled = false

    def cancel(eventIfRuning: Boolean): Boolean = false
  }

}

case class Map2Future[A, B, C](a: JFuture[A], b: JFuture[B], f: (A, B) => C)
    extends JFuture[C] {

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
