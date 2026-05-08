lazy val root = project
  .in(file("."))
  .settings(
    name := "Yafl",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := "3.8.3",
    libraryDependencies += "org.scalameta" %% "munit" % "1.3.0" % Test,
    scalacOptions ++= Seq("-deprecation")
  )
