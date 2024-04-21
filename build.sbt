val scala3Version = "3.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name                                   := "pps-23-24-lab06",
    version                                := "0.1.0-SNAPSHOT",
    scalaVersion                           := scala3Version,
    libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.2" % Test
  )
