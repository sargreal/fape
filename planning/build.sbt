libraryDependencies ++= Seq(
  "com.martiansoftware" % "jsap" % "2.1",
  "de.sciss" % "prefuse-core" % "1.0.0",
  "org.projectlombok" % "lombok" % "1.18.26",

  // slf4j
  "org.slf4j" % "slf4j-api" % "1.7.30",

  // progress bar
  "me.tongfei" % "progressbar" % "0.9.5",
)


mainClass := Some("fr.laas.fape.planning.Planning")

exportJars := true



