// <usage overhaul>.<breaking code change (including fixes)>.<user facing addition>
ThisBuild / version := "0.19.0"
ThisBuild / organization := "be.adamv"
ThisBuild / scalaVersion := "3.2.1"
ThisBuild / compileOrder := CompileOrder.JavaThenScala

val publishSettings = Seq(
  publishTo := Some(Resolver.file("local-ivy", file("~"))),
)

lazy val root = crossProject(JSPlatform, JVMPlatform, NativePlatform).withoutSuffixFor(JVMPlatform)
  .aggregate(core, collection)
  .settings(
    name := "macroloop",
    publish / skip := true
  )

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform).withoutSuffixFor(JVMPlatform)
  .in(file("core"))
  .settings(publishSettings)
  .settings(
    name := "macroloop-core",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
  )

lazy val collection = crossProject(JSPlatform, JVMPlatform, NativePlatform).withoutSuffixFor(JVMPlatform)
  .in(file("collection"))
  .dependsOn(core % "compile->compile;test->test;provided->provided")
  .settings(publishSettings)
  .settings(
    name := "macroloop-collection",
  )

lazy val bench = crossProject(JVMPlatform).withoutSuffixFor(JVMPlatform).crossType(CrossType.Pure)
  .in(file("bench"))
  .dependsOn(core % "compile->compile;test->test;provided->provided")
  .enablePlugins(JmhPlugin)

lazy val staging = crossProject(JVMPlatform).withoutSuffixFor(JVMPlatform).crossType(CrossType.Pure)
  .in(file("staging"))
  .dependsOn(core % "compile->compile;test->test;provided->provided")
  .settings(publishSettings)
  .settings(
    name := "macroloop-staging",
    libraryDependencies += "org.scala-lang" %% "scala3-staging" % scalaVersion.value
  )
