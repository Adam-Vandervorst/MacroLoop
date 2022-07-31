ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.1-RC1-bin-20220623-5a8a61d-NIGHTLY"


lazy val root = project.in(file("."))
  .aggregate(core, staging, collection)
  .settings(
    name := "macroloop",
    compileOrder := CompileOrder.JavaThenScala
  )

lazy val core = project.in(file("core"))
  .settings(
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
  )

lazy val bench = project.in(file("core/src/bench"))
  .dependsOn(core)
  .enablePlugins(JmhPlugin)

lazy val staging = project.in(file("staging"))
  .dependsOn(core % "compile->compile;test->test;provided->provided")
  .settings(
    name := "macroloop-staging",
    libraryDependencies += "org.scala-lang" %% "scala3-staging" % scalaVersion.value
  )

lazy val collection = project.in(file("collection"))
  .dependsOn(core % "compile->compile;test->test;provided->provided")
  .settings(
    name := "macroloop-collection",

)

//Jmh / sourceDirectory := (Test / sourceDirectory).value
//Jmh / classDirectory := (Test / classDirectory).value
//Jmh / dependencyClasspath := (Test / dependencyClasspath).value
//// rewire tasks, so that 'jmh:run' automatically invokes 'jmh:compile' (otherwise a clean 'jmh:run' would fail)
//Jmh / compile := (Jmh / compile).dependsOn(Test / compile).value
//Jmh / run := (Jmh / run).dependsOn(Jmh / Keys.compile).evaluated