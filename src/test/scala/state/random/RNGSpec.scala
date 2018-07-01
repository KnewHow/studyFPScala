package test.fpscala.state

import org.scalatest._
import fpscala.state._

class RNGSpec extends FlatSpec {

  "implement random number generator" should "success" in {
    val rng = SimpleRNG(42)
    val (n1, r1) = rng.nextInt
    val (n2,r2) = r1.nextInt
    println("n1" -> n1)
    println("n2" -> n2)
    assert(true)
  }

  "implement nonNegativeInt function with toInt function" should "success" in {
    val rng = SimpleRNG(42)
    val (n1, r1) = rng.nonNegativeInt(rng)
    val (n2, r2) = r1.nonNegativeInt(r1)
    val (n3, r3) = r2.nonNegativeInt(r2)
    println(s"n1, r1 -> ${n1} -> ${r1}")
    println(s"n2, r2 -> ${n2} -> ${r2}")
    println(s"n3, r3 -> ${n3} -> ${r3}")
  }

  "implement double funcion with nonNegativeInt function" should "success" in {
    val rng = SimpleRNG(42)
    val (n1, r1) = rng.double(rng)
    val (n2, r2) = r1.double(r1)
    val (n3, r3) = r2.double(r2)
    println(s"double test -> \n")
    println(s"n1, r1 -> ${n1} -> ${r1}")
    println(s"n2, r2 -> ${n2} -> ${r2}")
    println(s"n3, r3 -> ${n3} -> ${r3}")
  }

  "implemt intDouble function" should "success" in {
    val r = SimpleRNG(42)
    val (v, r1) = r.intDouble(r)
    println(s"intDouble function \n")
    println(s"v -> $v, r1 -> $r1")
    assert(true)
  }

  "implent double int funciton" should "success" in {
    val r = SimpleRNG(42)
    val (v, r1)  = r.doubleInt(r)
    println(s"double int f \n")
    println(s"v -> $v")
    assert(true)
  }

  "implemt dounle3 function" should "success" in {
    val r = SimpleRNG(42)
    val (v, r1) = r.double3(r)
    println(s"dounle3 test\n")
    println(s"v -> $v")
    assert(true)
  }

  "implement ints function" should "success" in {
    val r = SimpleRNG(42)
    val rs = r.ints(2)(r)
    println(rs._1)
    assert(true)
  }
}
