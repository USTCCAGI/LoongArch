// See README.md for license details.

ThisBuild / scalaVersion     := "2.13.12" 
ThisBuild / version          := "0.1.0"
ThisBuild / organization     := "USTC"

val chiselVersion = "6.3.0"

lazy val root = (project in file("."))
  .settings(
    name := "USTCCAGI",
    libraryDependencies ++= Seq(
      "org.chipsalliance" %% "chisel" % chiselVersion,
      // "edu.berkeley.cs" %% "chiseltest" % chiselVersion
    ),
    scalacOptions ++= Seq(
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit",
      "-Ymacro-annotations",
      "-opt:inline:**",
      // "-P:chiselplugin:genBundleElements",
    ),
    addCompilerPlugin("org.chipsalliance" % "chisel-plugin" % chiselVersion cross CrossVersion.full),
  )

