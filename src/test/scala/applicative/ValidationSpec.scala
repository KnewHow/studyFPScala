package test.fpscala.applicative

import fpscala.applicative._
import org.scalatest.FlatSpec
import fpscala.basic.Logger.Logger
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

class ValidationSpec extends FlatSpec {
  def validateName(name: String): Validation[String, String] =
    if (name.isEmpty) {
      Failure("name don't be empty")
    } else {
      Success(name)
    }

  def validateBirthdate(birthdate: String): Validation[String, DateTime] = {
    try {
      val format = DateTimeFormat.forPattern("yyyy-MM-dd")
      val d      = DateTime.parse(birthdate, format)
      Success(d)
    } catch {
      case ex: Throwable => Failure("birthdate must in the form yyyy-MM-dd")
    }
  }

  def validatePhone(phoneNumber: String): Validation[String, String] =
    if (phoneNumber.matches("[0-9]{11}")) {
      Success(phoneNumber)
    } else {
      Failure("Phone number must at 11 digits")
    }

  case class WebForm(name: String, birthdate: DateTime, phoneNumber: String)

  "test validation applicative" should "succeed" in {
    Validation.validationApplicative.map3(
      validateName("How"),
      validateBirthdate("wqwqwwq"),
      validatePhone("12sx")
    )(WebForm(_, _, _)) match {
      case Failure(h, t) =>
        Logger.info(s"failure head is -> $h,tail is -> $t")
        succeed
      case Success(r) =>
        Logger.info(s"success result -> $r")
        succeed
    }
  }
}
