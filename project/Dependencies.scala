import sbt._

object Dependencies {
  val scala = "2.13.2"

  val cats = "2.1.1"
  val catsEffect = "2.1.2"
  val fs2 = "2.2.1"
  val circe = "0.13.0"
  def diffson = "4.0.2"
  val fastParse = "2.3.0"
  val scalaLogging = "3.9.2"
  val kamon = "2.1.0"

  val scalatest = "3.1.2"
  val scalacheck = "1.14.3"
  val scalatestScalacheck = "3.1.2.0"
  val scalacheckShapeless = "1.2.5"

  val logBinding: Seq[ModuleID] = Seq(
    "org.slf4j" % "jul-to-slf4j" % "1.7.30",
    "org.slf4j" % "log4j-over-slf4j" % "1.7.30",
    "ch.qos.logback" % "logback-classic" % "1.2.3"
  )
}
