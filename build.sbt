import xerial.sbt.Sonatype._

lazy val V = _root_.scalafix.sbt.BuildInfo

val Scala212Version = "2.12.19"
val Scala213Version = "2.13.13"
val Scala3Version = "3.3.3"

inThisBuild(
  List(
    organization := "org.virtuslab",
    homepage := Some(url("https://github.com/VirtusLabRnD/scalafix-migrate-zio-macros")),
    licenses := List(
      "Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")
    ),
    developers := List(
      Developer("WojciechMazur", "Wojciech Mazur", "wmazur@virtuslab.com", url("https://github.com/WojciechMazur"))
    ),
    version := "0.1.0",
    scalaVersion := Scala213Version,
    semanticdbEnabled := true,
    semanticdbIncludeInJar := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    versionScheme := Some("early-semver")
  )
)

Global / PgpKeys.pgpPassphrase := sys.env.get("PGP_PW").map(_.toCharArray())
// Global / PgpKeys.pgpSigningKey := Some("BCE7DB09E1B2687C9C9C3AB2D8DF100359D36CBF")
Global / publishTo := sonatypePublishToBundle.value
Global / credentials ++= (
  for {
    username <- sys.env.get("SONATYPE_USER")
    password <- sys.env.get("SONATYPE_PW")
  } yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)
).toList
Global / scmInfo := Some(
  ScmInfo(
    url("https://github.com/VirtusLabRnD/scalafix-migrate-zio-macros"),
    "scm:git@github.com:VirtusLabRnD/scalafix-migrate-zio-macros.git"
  )
)

lazy val rules = project.settings(
  moduleName := "scalafix-migrate-zio-macros",
  libraryDependencies += "ch.epfl.scala" %% "scalafix-core" % V.scalafixVersion,
  crossScalaVersions := Seq(Scala212Version, Scala213Version)
)

val ZIOVersion = "2.0.21"
val ZIOMockVersion = "1.0.0-RC11"

lazy val commonTestDependencies = List(
  "dev.zio" %% "zio" % ZIOVersion,
  "dev.zio" %% "zio-managed" % ZIOVersion,
  "dev.zio" %% "zio-mock" % ZIOMockVersion,

)

lazy val input = project.settings(
  (publish / skip) := true,
  scalaVersion := Scala213Version,
  scalacOptions ++= Seq(
    "-Ymacro-annotations"
  ),
  libraryDependencies ++= commonTestDependencies ++ Seq(
    "dev.zio" %% "zio-macros" % ZIOVersion,
  )
)

lazy val output = project.settings(
  (publish / skip) := true,
  scalaVersion := Scala3Version,
  libraryDependencies ++= commonTestDependencies
)

lazy val tests = project
  .settings(
    crossScalaVersions := Seq(Scala212Version, Scala213Version),
    (publish / skip) := true,
    libraryDependencies += "ch.epfl.scala" % "scalafix-testkit" % V.scalafixVersion % Test cross CrossVersion.full,
    scalafixTestkitOutputSourceDirectories := (output / Compile / unmanagedSourceDirectories).value,
    scalafixTestkitInputSourceDirectories := (input / Compile / unmanagedSourceDirectories).value,
    scalafixTestkitInputClasspath := (input / Compile / fullClasspath).value
  )
  .dependsOn(rules)
  .enablePlugins(ScalafixTestkitPlugin)
