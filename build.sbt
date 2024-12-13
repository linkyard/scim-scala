import sbt.Keys.libraryDependencies
ThisBuild / scalaVersion := Dependencies.scala
ThisBuild / organization := "ch.linkyard.scim"
ThisBuild / organizationName := "linkyard ag"

ThisBuild / githubOwner := "linkyard"
ThisBuild / githubRepository := "scim-scala"

githubTokenSource := TokenSource.Environment("GITHUB_TOKEN") || TokenSource.GitConfig("github.token")

lazy val root = (project in file("."))
  .settings(
    name := "scim-scala",
    inThisBuild(List(
      Global / onChangedBuildSource := ReloadOnSourceChanges,
      semanticdbEnabled := true,
      semanticdbVersion := scalafixSemanticdb.revision,
      scalacOptions += "-source:3.0-migration",
      scalacOptions += "-new-syntax",
      scalacOptions += "-rewrite",
      scalacOptions += "-feature",
      scalacOptions += "-unchecked",
      scalacOptions += "-deprecation",
//      scalacOptions += "-Wconf:any:e",
//      scalacOptions += "-Wconf:id=E198:w",
      scalacOptions += "-Wvalue-discard",
      scalacOptions += "-Wunused:all",
      scalacOptions ++= Seq("-Xmax-inlines", "50"),
      ThisBuild / turbo := true,
      Test / logBuffered := false,
      cancelable in Global := true,
      githubTokenSource := TokenSource.GitConfig("github.token"),
      publish := {},
      libraryDependencies ++= Seq(
        "io.kamon" %% "kamon-core" % Dependencies.kamon,
        "com.typesafe.scala-logging" %% "scala-logging" % Dependencies.scalaLogging,
      ),
      libraryDependencies ++= Seq(
        "org.scalatest" %% "scalatest" % Dependencies.scalatest % Test,
        "org.scalacheck" %% "scalacheck" % Dependencies.scalacheck % Test,
        "org.scalatestplus" %% "scalacheck-1-16" % Dependencies.scalatestScalacheck % Test,
      ),
      libraryDependencies ++= Dependencies.logBinding.map(_ % Test),
    )))
  .aggregate(
    core
  )

lazy val core = (project in file("core"))
  .settings(
    name := "scim-scala-core",
    githubTokenSource := TokenSource.GitConfig("github.token"),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % Dependencies.cats,
      "io.circe" %% "circe-generic" % Dependencies.circe,
      "io.circe" %% "circe-optics" % "0.15.0",
      "com.lihaoyi" %% "fastparse" % Dependencies.fastParse,
      "io.circe" %% "circe-parser" % Dependencies.circe,
      "io.circe" %% "circe-testing" % Dependencies.circe % Test,
    ),
    libraryDependencies ++= Dependencies.logBinding,
  )

