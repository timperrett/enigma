
scalaVersion := "2.11.4"

name := "enigma"

organization := "com.timperrett"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.1.0",
  "org.scalacheck" %% "scalacheck" % "1.11.6" % "test"
)

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-unchecked",
  "-feature",
  "-language:implicitConversions",
  "-language:higherKinds"
)
