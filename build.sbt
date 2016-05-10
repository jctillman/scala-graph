lazy val root = (project in file(".")).
  settings(
    name := "agi",
    version := "0.0.1",
    scalaVersion := "2.11.7"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"
libraryDependencies += "com.sksamuel.scrimage" %% "scrimage-core" % "2.1.0"
libraryDependencies += "com.sksamuel.scrimage" %% "scrimage-io-extra" % "2.1.0"
libraryDependencies += "com.sksamuel.scrimage" %% "scrimage-filters" % "2.1.0"