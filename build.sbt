name := "book-fp-in-scala"
scalaVersion := "2.12.1"

libraryDependencies += "org.specs2" %% "specs2-core" % "3.8.9" % Test

fork in Test := true
