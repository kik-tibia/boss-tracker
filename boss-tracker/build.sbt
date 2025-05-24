ThisBuild / version := "4.4.0"

ThisBuild / scalaVersion := "3.7.0"

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
val doobieVersion = "1.0.0-RC8"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)

libraryDependencies ++= Seq(
  "org.typelevel" %% "log4cats-slf4j" % "2.5.0",
  "co.fs2" %% "fs2-core" % "3.7.0",
  "org.apache.commons" % "commons-text" % "1.9",
  "ch.qos.logback" % "logback-classic" % "1.2.10",
  "is.cir" %% "ciris" % "3.1.0",
  "net.dv8tion" % "JDA" % "5.0.0-beta.12",
  "org.tpolecat" %% "doobie-core" % doobieVersion,
  "org.tpolecat" %% "doobie-postgres" % doobieVersion,
  "org.postgresql" % "postgresql" % "42.5.3"
)
