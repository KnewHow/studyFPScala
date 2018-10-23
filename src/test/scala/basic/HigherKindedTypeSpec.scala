package test.fpscala.basic

import org.scalatest.FlatSpec

class ScalaHigherKindedSpec extends FlatSpec {
  // 定义一个Container trait，它拥有一个高等类型
  trait Container[M[_]] {
    def put[A](a: A): M[A]
    def get[A](a: M[A]): A
  }

  trait Container2[M[- _]]

  // 把 List[A] 作为类型传递给 Container
  case object MyList extends Container[List] {
    def put[A](a: A): List[A] = List(a)
    def get[A](a: List[A]): A = a.head
  }

  // 把 Some[A] 作为类型传递给 Container
  case object MySome extends Container[Some] {
    def put[A](a: A): Some[A] = Some(a)
    def get[A](a: Some[A]): A = a.get
  }

  // 测试
  "test MyList" should "succeed" in {
    val ml = MyList.put(1)
    assert(MyList.get(ml) == 1)
  }

  "test MySome" should "succeed" in {
    val ms = MySome.put(2)
    assert(MySome.get(ms) == 2)
  }
}
