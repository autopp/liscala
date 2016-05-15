lazy val root = (project in file(".")).
  settings(
    name := "lisp",
    version := "0.1"
  )
libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.2.1" % "test"
