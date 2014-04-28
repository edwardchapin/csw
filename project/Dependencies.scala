import sbt._

// Dependencies

object Dependencies {

  val akkaVersion = "2.3.0"
  val sprayVersion = "1.3.0"
  val hornetqVersion = "2.4.0.Final"
//  val hornetqVersion = "2.5.0-SNAPSHOT"

  def compile   (deps: ModuleID*): Seq[ModuleID] = deps map (_ % "compile")
  def provided  (deps: ModuleID*): Seq[ModuleID] = deps map (_ % "provided")
  def test      (deps: ModuleID*): Seq[ModuleID] = deps map (_ % "test")
  def runtime   (deps: ModuleID*): Seq[ModuleID] = deps map (_ % "runtime")
  def container (deps: ModuleID*): Seq[ModuleID] = deps map (_ % "container")

  val akkaActor      = "com.typesafe.akka"             %% "akka-actor"            % akkaVersion
  val akkaKernel     = "com.typesafe.akka"             %% "akka-kernel"           % akkaVersion
  val akkaRemote     = "com.typesafe.akka"             %% "akka-remote"           % akkaVersion
  val akkaZeromq     = "com.typesafe.akka"             %% "akka-zeromq"           % akkaVersion

  val typesafeConfig = "com.typesafe"                   % "config"                % "1.2.0"
  val scalaLogging   = "com.typesafe"                  %% "scalalogging-slf4j"    % "1.1.0"
  val logback        = "ch.qos.logback"                 % "logback-classic"       % "1.1.1"

  val sprayCan       = "io.spray"                       % "spray-can"             % sprayVersion
  val sprayClient    = "io.spray"                       % "spray-client"          % sprayVersion
  val sprayRouting   = "io.spray"                       % "spray-routing"         % sprayVersion
  val sprayJson      = "io.spray"                      %% "spray-json"            % "1.2.5"
  val sprayTestkit   = "io.spray"                       % "spray-testkit"         % sprayVersion

  val jgit           = "org.eclipse.jgit"               % "org.eclipse.jgit"      % "3.3.1.201403241930-r"
  val scalaIoFile    = "com.github.scala-incubator.io" %% "scala-io-file"         % "0.4.2"

  val redisScala     = "com.etaty.rediscala"           %% "rediscala"             % "1.3akka2.3" // XXX TEMP FIX

  val hornetqServer  = "org.hornetq"                    % "hornetq-server"        % hornetqVersion
  // XXX TEMP FIX (http://stackoverflow.com/questions/21882100/adding-hornetq-dependency-in-sbt-gives-resolution-failure-for-hornetq-native-n)
  val hornetqNative  = "org.hornetq"                    % "hornetq-native"        % hornetqVersion from s"http://repo1.maven.org/maven2/org/hornetq/hornetq-native/$hornetqVersion/hornetq-native-$hornetqVersion.jar"
//  val hornetqNative  = "org.hornetq"                    % "hornetq-native"        % hornetqVersion from s"${Path.userHome.asFile.toURI.toURL}.m2/repository/org/hornetq/hornetq-native/$hornetqVersion/hornetq-native-$hornetqVersion.jar"

  // Test dependencies
  val akkaTestKit    = "com.typesafe.akka"             %% "akka-testkit"          % akkaVersion
  val akkaMultiNodeTest ="com.typesafe.akka"           %% "akka-multi-node-testkit" % akkaVersion
  val scalaTest      = "org.scalatest"                  % "scalatest_2.10"        % "2.1.0"
  val junit          = "com.novocode"                   % "junit-interface"       % "0.10"
  val specs2         = "org.specs2"                    %% "specs2"                % "2.3.10"
  val liftJSON       = "net.liftweb"                   %% "lift-json"             % "2.5.1"


}

