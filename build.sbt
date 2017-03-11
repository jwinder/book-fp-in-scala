name := "example"
scalaVersion := "2.12.1"

libraryDependencies += "org.specs2" %% "specs2-core" % "3.8.9" % Test

initialCommands in console += List("\nimport example.data._",
                                   "\nimport example.chapter2._",
                                   "\nimport example.chapter3._").mkString
