libraryDependencies ++= Seq(
  "joda-time" % "joda-time" % "2.9.1",
  "org.joda" % "joda-convert" % "1.2", // this optional dependency of joda time is required by scala compiler
  "org.scalatest" %% "scalatest" % "2.2.4" % "test"
)

lazy val root = (project in file(".")).
    settings(
      name := "sparse",
      version := "1.0",
      scalaVersion := "2.10.5"
    )