name := "raytracer-challenge"

version := "0.1"

scalaVersion := "2.13.1"

enablePlugins(ScalaJSPlugin)
scalaJSUseMainModuleInitializer := true

libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0" % "test"
libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.8"
