import Dependencies._

resolvers += Resolver.bintrayRepo("hseeberger", "maven")
scalacOptions += "-Xmacro-settings:materialize-derivations"

lazy val root = (project in file(".")).
		settings(
			inThisBuild(List(
				organization := "org.jro",
				scalaVersion := "2.13.7",
				version      := "1.0.0"
			)),
			name := "Advent of code",
			scalacOptions ++= Seq(
				"-language:postfixOps",
				"-Xmacro-settings:materialize-derivations"
			),
			libraryDependencies ++= Seq(
				scalaTest % Test
			)
		)
