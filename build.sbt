val dottyVersion = "3.0.0-M1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "aoc2020",
    version := "0.1.0",

    scalaVersion := dottyVersion,
    scalacOptions ++= Seq(
      "-Yexplicit-nulls",
      "-language:strict-equality",
    ),
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.3.0",
  )
  .enablePlugins(JmhPlugin)

Runtime / unmanagedSources += baseDirectory.value / "input"
