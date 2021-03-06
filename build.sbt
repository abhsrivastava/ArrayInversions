lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.6",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "ArrayInversions",
    fork := true,
    javaOptions ++= Seq("-Xmx20G", "-Xss128M")
  )
