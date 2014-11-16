
scalaVersion := "2.11.4"

name := "enigma"

organization := "com.timperrett"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.0"

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-unchecked",
  "-feature",
  "-language:implicitConversions",
  "-language:higherKinds"
)
