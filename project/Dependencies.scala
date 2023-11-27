import sbt._

object Dependencies {
	val akkaVersion = "2.6.8" // TODO update
	val sprayVersion = "10.2.1" // TODO update
	
	lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.2.15"
	lazy val akkaHttp = "com.typesafe.akka" %% "akka-http" % sprayVersion
	lazy val akkaHttpTestkit = "com.typesafe.akka" %% "akka-http-testkit" % sprayVersion
	lazy val akkaActor = "com.typesafe.akka" %% "akka-actor" % akkaVersion
	lazy val akkaTyped = "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion
	lazy val akkaTestkit = "com.typesafe.akka" %% "akka-testkit" % akkaVersion
	lazy val akkaStream = "com.typesafe.akka" %% "akka-stream" % akkaVersion
	lazy val akkaStreamTestkit = "com.typesafe.akka" %% "akka-stream-testkit" % akkaVersion
	lazy val playJson = "com.typesafe.play" %% "play-json" % "2.7.4"
	lazy val akkHttpPlayJson = "de.heikoseeberger" %% "akka-http-play-json" % "1.31.0"
	lazy val lightbendConfig = "com.typesafe" % "config" % "1.4.0"
	lazy val pureconfig = "com.github.pureconfig" %% "pureconfig" % "0.14.0"
	lazy val csvParser = "io.kontainers" %% "purecsv" % "0.4.0"

}
