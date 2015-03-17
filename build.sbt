name := "wundercal"

version := "1.0"

scalaVersion := "2.11.6"

herokuAppName in Compile := "wundercal"

lazy val root = (project in file(".")).enablePlugins(PlayJava)

libraryDependencies ++= Seq(
  ws,
  "org.mnode.ical4j" % "ical4j" % "1.0.2"
)