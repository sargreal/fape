name := "fape-meta"

// global settings 

inThisBuild(List(
    scalaVersion := "2.12.18",
    // These are normal sbt settings to configure for release, skip if already defined
    organization := "com.github.arthur-bit-monnot",
    licenses := Seq("BSD-2-Clause" -> url("https://opensource.org/licenses/BSD-2-Clause")),
    homepage := Some(url("https://github.com/arthur-bit-monnot/fape")),
    developers := List(Developer("arthur-bit-monnot", "Arthur Bit-Monnot", "arthur.bitmonnot@gmail.com", url("https://arthur-bit-monnot.github.io"))),
    scmInfo := Some(ScmInfo(url("https://github.com/arthur-bit-monnot/fape"), "scm:git:git@github.com:arthur-bit-monnot/fape.git")),

    // These are the sbt-release-early settings to configure
    pgpPublicRing := file("./travis/local.pubring.asc"),
    pgpSecretRing := file("./travis/local.secring.asc"),
    releaseEarlyEnableLocalReleases := true,
    releaseEarlyWith := SonatypePublisher
))

lazy val commonSettings = Seq(
  crossPaths := true,
  exportJars := true, // insert other project dependencies / oneJar
  run / fork := true,
  run / javaOptions ++= Seq("-Xmx8000m", "-ea"),
  compile / javacOptions  ++= Seq("-Xlint"),
  javacOptions ++= Seq("-source", "1.8", "-target", "1.8"),
  doc / javacOptions ++= Seq("-Xdoclint:none"),
  scalacOptions ++= Seq(
    "-opt:l:method",
    "-Xdisable-assertions"
  ),
  assembly / test := {},
  assembly / assemblyMergeStrategy := {
    case PathList("org", "w3c", xs @ _*)         => MergeStrategy.first
    case x =>
      val oldStrategy = (assembly / assemblyMergeStrategy).value
      oldStrategy(x)
  },
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test",
)

lazy val root = project.in(file(".")).
  aggregate(fapeActing, fapePlanning, constraints, anml, svgPlot, structures).

  settings(
    publish := {},

    publishLocal := {}
  )

lazy val fapeActing = Project("fape-acting", file("acting"))
     .dependsOn(fapePlanning, constraints, anml, svgPlot, structures)
     .settings(commonSettings: _*)

lazy val fapePlanning = Project("fape-planning", file("planning"))
     .dependsOn(constraints, anml, svgPlot, structures)
     .settings(commonSettings: _*)
     .settings(crossPaths := false)  // disable cross path as this is a pure java project

lazy val constraints = Project("fape-constraints", file("constraints"))
     .dependsOn(anml, structures)
     .settings(commonSettings: _*)

lazy val anml = Project("fape-anml-parser", file("anml-parser"))
     .dependsOn(structures)
     .settings(commonSettings: _*)
     .settings(libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4")

lazy val svgPlot = Project("fape-svg-plot", file("svg-plot"))
     .settings(commonSettings: _*)
     .settings(libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.6")

lazy val structures = Project("fape-structures", file("structures"))
     .settings(commonSettings: _*)


