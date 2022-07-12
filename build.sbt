ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.1-RC1-bin-20220623-5a8a61d-NIGHTLY"

ThisBuild / compileOrder := CompileOrder.JavaThenScala


lazy val root = (project in file("."))
  .settings(
    name := "MacroLoop",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    libraryDependencies += "org.scala-lang" %% "scala3-staging" % scalaVersion.value
  ).enablePlugins(JmhPlugin)

lazy val bench = project.in(file("src/bench")).dependsOn(root).enablePlugins(JmhPlugin)
//Jmh / sourceDirectory := (Test / sourceDirectory).value
//Jmh / classDirectory := (Test / classDirectory).value
//Jmh / dependencyClasspath := (Test / dependencyClasspath).value
//// rewire tasks, so that 'jmh:run' automatically invokes 'jmh:compile' (otherwise a clean 'jmh:run' would fail)
//Jmh / compile := (Jmh / compile).dependsOn(Test / compile).value
//Jmh / run := (Jmh / run).dependsOn(Jmh / Keys.compile).evaluated