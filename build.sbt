name := "wundercal"

version := "1.0"

scalaVersion := "2.11.6"

herokuAppName in Compile := "wundercal"

lazy val root = (project in file(".")).enablePlugins(PlayJava)

libraryDependencies ++= Seq(
  ws,
  "org.mnode.ical4j" % "ical4j" % "1.0.2",
  "com.typesafe.play.extras" %% "iteratees-extras" % "1.4.0",
  "com.newrelic.agent.java" % "newrelic-agent" % "3.9.0"
)