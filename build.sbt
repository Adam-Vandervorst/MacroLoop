// <usage overhaul>.<breaking code change (including fixes)>.<user facing addition>
ThisBuild / version := "0.12.7"
ThisBuild / organization := "be.adamv"
ThisBuild / scalaVersion := "3.2.0"

val publishSettings = Seq(
  publishTo := Some(Resolver.file("local-ivy", file("~"))),
)

lazy val root = project.in(file("."))
  .aggregate(core, staging, collection)
  .settings(
    name := "macroloop",
    compileOrder := CompileOrder.JavaThenScala,
    publish / skip := true
  )

lazy val core = project.in(file("core"))
  .settings(publishSettings)
  .settings(
    name := "macroloop-core",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
  )

lazy val bench = project.in(file("core/src/bench"))
  .dependsOn(core)
  .enablePlugins(JmhPlugin)

lazy val staging = project.in(file("staging"))
  .dependsOn(core % "compile->compile;test->test;provided->provided")
  .settings(publishSettings)
  .settings(
    name := "macroloop-staging",
    libraryDependencies += "org.scala-lang" %% "scala3-staging" % scalaVersion.value
  )

lazy val collection = project.in(file("collection"))
  .dependsOn(core % "compile->compile;test->test;provided->provided")
  .settings(publishSettings)
  .settings(
    name := "macroloop-collection",
)
