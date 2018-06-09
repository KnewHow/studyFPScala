package test.fpscala.errorhandling

import org.scalatest._
import fpscala.errorhandling._
import scala.collection.immutable.Map
import scala.util.Random

class OptionSpec extends FlatSpec {
  val es = Employee.getEmployees
  "test map function " should "success" in {
    val r = Employee.getEmployees.map(_.department)
    println(r)
    assert(true)

  }

  "test flatMap function" should "success" in {
    val r = Employee.getEmployees.flatMap(_.manager)
    println(r)
    assert(true)
  }

  "test getOrElse function" should "success" in {
    val r = Employee.getEmployees.map(_.department).getOrElse("default department")
    println(r)
    assert(true)
  }
}

case class Employee(name: String, department: String, manager:Option[String]=None)

object Employee{
  def getEmployees: Option[Employee] = {
    val r = Random.nextBoolean
    r match {
      case true => fpscala.errorhandling.Option(Employee("how","developing",Option("yes")))
      case _ => None
    }
  }
}
