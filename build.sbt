name := "sclcode"

version := "0.1"

scalaVersion := "3.0.0-M1"

scalacOptions ++= Seq("-deprecation")

libraryDependencies += ("org.scalactic" %% "scalactic" % "3.2.2").withDottyCompat(scalaVersion.value)
libraryDependencies += ("org.scalatest" %% "scalatest" % "3.2.2" % "test").withDottyCompat(scalaVersion.value)
