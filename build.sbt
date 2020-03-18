import sbt.Keys.libraryDependencies
ThisBuild / scalaVersion := Dependencies.scala
ThisBuild / version := "dev-SNAPSHOT"
ThisBuild / organization := "ch.linkyard.scim"
ThisBuild / organizationName := "linkyard ag"

lazy val root = (project in file("."))
  .settings(
    name := "scim-scala",
    inThisBuild(List(
      Global / onChangedBuildSource := ReloadOnSourceChanges,
      scalacOptions += "-unchecked",
      scalacOptions += "-deprecation",
      scalacOptions += "-Xfatal-warnings",
      scalacOptions += "-feature",
      scalacOptions += "-Ymacro-annotations",
      scalacOptions += "-language:higherKinds",
      ThisBuild / turbo := true,
      logBuffered in Test := false,
      addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full),
      addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
      cancelable in Global := true,
      libraryDependencies ++= Seq(
        "io.kamon" %% "kamon-core" % Dependencies.kamon,
        "com.typesafe.scala-logging" %% "scala-logging" % Dependencies.scalaLogging,
      ),
      libraryDependencies ++= Seq(
        "org.scalatest" %% "scalatest" % Dependencies.scalatest % Test,
        "org.scalacheck" %% "scalacheck" % Dependencies.scalacheck % Test,
        "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % Dependencies.scalacheckShapeless % Test,
      ),
      libraryDependencies ++= Dependencies.logBinding.map(_ % Test),
    )))
  .aggregate(
    scim
  )

lazy val scim = (project in file("scim"))
  .settings(
    name := "scim",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % Dependencies.cats,
      "org.typelevel" %% "cats-effect" % Dependencies.catsEffect,
      "io.circe" %% "circe-generic" % Dependencies.circe,
    ),
    libraryDependencies ++= Dependencies.logBinding,
  )

