name := "fp-scala"
version := "0.1.0"
scalaVersion := "2.12.6"

libraryDependencies ++= Seq("io.monix" %% "minitest" % "2.1.1" % "test")

testFrameworks += new TestFramework("minitest.runner.Framework")