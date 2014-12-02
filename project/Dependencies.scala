import sbt._

// Dependencies

object Dependencies {

  val scalaVersion = "2.11.4"
  val akkaVersion = "2.3.7"
  val sprayVersion = "1.3.2"
  val hornetqVersion = "2.4.0.Final"
  //  val hornetqVersion = "2.5.0-SNAPSHOT"

  def compile(deps: ModuleID*): Seq[ModuleID] = deps map (_ % "compile")

  def provided(deps: ModuleID*): Seq[ModuleID] = deps map (_ % "provided")

  def test(deps: ModuleID*): Seq[ModuleID] = deps map (_ % "test")

  def runtime(deps: ModuleID*): Seq[ModuleID] = deps map (_ % "runtime")

  def container(deps: ModuleID*): Seq[ModuleID] = deps map (_ % "container")

  val akkaActor = "com.typesafe.akka" %% "akka-actor" % akkaVersion
  val akkaKernel = "com.typesafe.akka" %% "akka-kernel" % akkaVersion
  val akkaRemote = "com.typesafe.akka" %% "akka-remote" % akkaVersion
  val akkaSlf4j = "com.typesafe.akka" %% "akka-slf4j" % akkaVersion
  // Akka streams (experimental)
  val akkaStream = "com.typesafe.akka" %% "akka-stream-experimental" % "0.11"
  val akkaHttp = "com.typesafe.akka" %% "akka-http-experimental" % "0.11"

  val jeromq = "org.zeromq" % "jeromq" % "0.3.3"
  val typesafeConfig = "com.typesafe" % "config" % "1.2.0"
  val scalaLogging = "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2"
  val logback = "ch.qos.logback" % "logback-classic" % "1.1.1"
  val janino = "org.codehaus.janino" % "janino" % "2.7.6"
  val logstashLogbackEncoder = "net.logstash.logback"   % "logstash-logback-encoder" % "3.1"

  val sprayCan = "io.spray" %% "spray-can" % sprayVersion
  val sprayHttpx = "io.spray" %% "spray-httpx" % sprayVersion
  val sprayClient = "io.spray" %% "spray-client" % sprayVersion
  val sprayRouting = "io.spray" %% "spray-routing" % sprayVersion
//  val sprayJson = "io.spray" %% "spray-json" % "1.2.6"
  val sprayJson = "io.spray" %% "spray-json" % "1.3.1"
  val sprayTestkit = "io.spray" %% "spray-testkit" % sprayVersion

  val jgit = "org.eclipse.jgit" % "org.eclipse.jgit" % "3.5.1.201410131835-r"

  val redisScala = "com.etaty.rediscala" %% "rediscala" % "1.3.1"

  val hornetqServer = "org.hornetq" % "hornetq-server" % hornetqVersion
  val hornetqNative = "org.hornetq" % "hornetq-native" % hornetqVersion from s"http://repo1.maven.org/maven2/org/hornetq/hornetq-native/$hornetqVersion/hornetq-native-$hornetqVersion.jar"

  val protobufJava = "com.google.protobuf" % "protobuf-java" % "2.5.0"

  // Test dependencies
  val akkaTestKit = "com.typesafe.akka" %% "akka-testkit" % akkaVersion
  val akkaMultiNodeTest = "com.typesafe.akka" %% "akka-multi-node-testkit" % akkaVersion
  val scalaTest = "org.scalatest" %% "scalatest" % "2.1.5"
  val junit = "com.novocode" % "junit-interface" % "0.10"
  val specs2 = "org.specs2" %% "specs2" % "2.3.11"

  // REPL dependencies
  val scalaLibrary = "org.scala-lang" % "scala-library" % scalaVersion
  val scalaCompiler = "org.scala-lang" % "scala-compiler" % scalaVersion
  val scalaReflect = "org.scala-lang" % "scala-reflect" % scalaVersion
  val jline = "jline" % "jline" % "2.11"
}

