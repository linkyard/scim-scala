import sbt._

object Dependencies {
  val scala = "3.5.2"

  val cats = "2.12.0"
  val catsEffect = "3.5.7"
  val fs2 = "3.11.1"
  val circe = "0.14.10"
  def diffson = "4.0.2"

  val fastParse = "3.1.1"
  val scalaLogging = "3.9.5"
  val kamon = "2.7.5"

  val scalatest = "3.2.19"
  val scalacheck = "1.18.1"
  val scalatestScalacheck = "3.2.14.0"
  val scalacheckShapeless = "1.3.1"

  val logBinding: Seq[ModuleID] = Seq(
    "org.slf4j" % "jul-to-slf4j" % "2.0.16",
    "org.slf4j" % "log4j-over-slf4j" % "2.0.16",
    "ch.qos.logback" % "logback-classic" % "1.5.12",
  )
}
