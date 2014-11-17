
scalaVersion := "2.11.2"

name := "enigma"

organization := "com.timperrett"

resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies ++= Seq(
  "org.scalaz"                 %% "scalaz-core"     % "7.1.0",
  "com.github.julien-truffaut" %% "monocle-core"    % "0.5.1",
  "com.github.julien-truffaut" %% "monocle-generic" % "0.5.1",
  "com.github.julien-truffaut" %% "monocle-macro"   % "0.5.1",
  "org.scalacheck"             %% "scalacheck"      % "1.11.6" % "test"
)

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-unchecked",
  "-feature",
  "-language:implicitConversions",
  "-language:higherKinds"
)
