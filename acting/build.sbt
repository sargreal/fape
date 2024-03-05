resolvers += "Akka library repository".at("https://repo.akka.io/maven")

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor-typed" % "latest.integration",

  // akka to use slf4j, config in resources/application.conf
  "com.typesafe.akka" %% "akka-slf4j" % "latest.integration",

  // Logger implementation for slf4j. Configuration in resources/logback.xml
  "ch.qos.logback" % "logback-classic" % "1.1.2",

  // CLI parsing
  "com.github.scopt" %% "scopt" % "4.1.0"

  // to make common logging (used by rosjava) use slf4j. Sees resources/common-properties
  //  "org.slf4j" % "jcl-over-slf4j" % "1.7.10"
)

mainClass := Some("fr.laas.fape.acting.Acting")

exportJars := true
