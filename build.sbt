name := "sws"

version := "0.1"

scalaVersion := "2.12.8"

//val scalazVersion = "7.2.27"
val scalazVersion = "7.1.17"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-zio" % "0.6.0",
  "org.scalaz" %% "scalaz-core" % scalazVersion,
  "org.scalaz" %% "scalaz-effect" % scalazVersion,
  "org.scalaz" %% "scalaz-typelevel" % scalazVersion,
  "org.scalaz" %% "scalaz-scalacheck-binding" % scalazVersion,
  "org.scalactic" %% "scalactic" % "3.0.5",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test",
)

parallelExecution in Test := false

scalacOptions += "-feature"
