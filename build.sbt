name := "uw-scala"

organization := "com.persist"

version := "0.2.0"

scalaVersion := "2.11.8"

viewSettings

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

scalacOptions ++= Seq("-Yrangepos")

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "com.persist" % "persist-json_2.11" % "1.1.3",
  //"org.mapdb" % "mapdb" % "2.0-beta12",
  "org.mapdb" % "mapdb" % "3.0.1",
  "jline" % "jline" % "2.14.2",
  "org.specs2" %% "specs2-core" % "3.8.5" % "test"
)

initialCommands += """
  import com.persist.uw.examples._
  import com.persist.JsonOps._
  import com.persist.JsonMapper._
  import com.persist.json._
  import scala.concurrent.duration.FiniteDuration
  import scala.concurrent.duration._
  import scala.language.postfixOps
                     """

