import Dependencies._

resolvers += Resolver.bintrayRepo("hseeberger", "maven")
scalacOptions ++= Seq("--explain", "--print-lines", "-Xprint-inline")

lazy val root = (project in file(".")).
		settings(
			inThisBuild(List(
				organization := "org.jro",
				scalaVersion := "3.4.3",
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
