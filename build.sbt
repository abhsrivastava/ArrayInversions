lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.6",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "ArrayInversions",
    javaOptions += "-Xms2G -Xmx24G -Xss1G -XX:+CMSClassUnloadingEnabled"
  )
