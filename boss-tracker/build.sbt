ThisBuild / version := "1.3.0"

ThisBuild / scalaVersion := "3.2.2"

name := "boss-tracker"

enablePlugins(DockerPlugin)
enablePlugins(JavaAppPackaging)
dockerExposedPorts += 443
Compile / mainClass := Some("com.kiktibia.bosstracker.tracker.Main")

scalacOptions ++= Seq(
  "-Xmax-inlines",
  "64" // https://github.com/circe/circe/issues/1760
)

val circeVersion = "0.14.3"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)

libraryDependencies ++= Seq(
  "org.typelevel" %% "log4cats-slf4j" % "2.5.0"
)
libraryDependencies += "co.fs2" %% "fs2-core" % "3.7.0"

libraryDependencies += "org.apache.commons" % "commons-text" % "1.9"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.10"

libraryDependencies += "is.cir" %% "ciris" % "3.1.0"

libraryDependencies += "net.dv8tion" % "JDA" % "5.0.0-beta.12"
