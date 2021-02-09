import sbt.ThisBuild

ThisBuild / name := "AdventOfCode2020"
ThisBuild / version := "0.0.1-SNAPSHOT"

lazy val commonSettings = Seq(
  ThisBuild / scalaVersion := "2.13.3",
  ThisBuild / scalacOptions ++= Seq(
    "-language:higherKinds",
    "-language:postfixOps"
  ),
)

lazy val root = (project in file("."))
        .aggregate(justScala, zio, cats)
        .settings(commonSettings)

lazy val justScala = project        
        .settings(commonSettings)
        .settings(libraryDependencies ++= Seq(
          "org.scalatest" %% "scalatest" % "3.2.3" % "test"
        ))

val zioVersion = "1.0.3"
lazy val zio = project
        .settings(commonSettings)
        .settings(libraryDependencies ++= Seq(
          "dev.zio" %% "zio" % zioVersion,
          "com.github.pathikrit" %% "better-files" % "3.9.1",
          "dev.zio" %% "zio-logging" % "0.5.4",

          "org.scalatest" %% "scalatest" % "3.2.3" % "test",
          "dev.zio" %% "zio-test" % zioVersion % "test",
          "dev.zio" %% "zio-test-sbt" % zioVersion % "test",
        ))
        .dependsOn(justScala)


lazy val cats = project
        .settings(commonSettings)
        .settings(libraryDependencies ++= Seq(
          "org.typelevel" %% "cats-core" % "2.0.0"
        ))
        .settings(addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full))



