scalaVersion := "2.12.4"
name := "studyFPScala"
organization := "com.github.knewhow"
version := "1.0"
libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.1"
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest"   % "3.0.5" % "test",
  "org.typelevel" %% "cats"        % "0.9.0",
  "org.slf4j"     % "slf4j-api"    % "1.7.5",
  "org.slf4j"     % "slf4j-simple" % "1.7.5"
)
scalafmtVersion in ThisBuild := "1.1.0"
scalafmtOnCompile in ThisBuild := true
