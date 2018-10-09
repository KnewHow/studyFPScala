scalaVersion := "2.12.4"
name := "studyFPScala"
organization := "knew.how"
version := "1.0"
libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.1"
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.5" % "test",
  "org.typelevel" %% "cats" % "0.9.0"
)
