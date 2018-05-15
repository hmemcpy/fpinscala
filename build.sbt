name := "fpinscala"
version := "0.1.0"
scalaVersion := "2.12.6"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "4.2.0" % "test",
  "org.specs2" %% "specs2-scalacheck" % "4.2.0" % "test",
  "org.scalacheck" %% "scalacheck" % "1.14.0" % "test")

scalacOptions in Test ++= Seq("-Yrangepos")
