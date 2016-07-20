package csw.examples

import akka.actor.{ActorRef, ActorSystem}
import csw.services.ccs.{AssemblyController, CommandStatus}
import csw.services.loc.Connection.AkkaConnection
import csw.services.loc.ConnectionType.AkkaType
import csw.services.loc.{ComponentId, ComponentType, Connection, LocationService}
import csw.services.pkg.Component.{AssemblyInfo, RegisterOnly}
import csw.services.pkg.Supervisor._
import csw.services.pkg.{Assembly, LifecycleHandler, Supervisor}
import csw.util.config.Configurations.{SetupConfig, SetupConfigArg}

import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * Class that implements the assembly actor
 *
 * @param info contains information about the assembly and the components it depends on
 */
class AssemblyExample(info: AssemblyInfo) extends Assembly with AssemblyController with LifecycleHandler {

  import AssemblyController._

  lifecycle(supervisor)

  // Get the connection to the HCD this assembly uses and track it
  trackConnections(info.connections)

  override def receive: Receive = controllerReceive orElse lifecycleHandlerReceive orElse {
    case x => log.error(s"Unexpected message: $x")
  }

  /**
   * Validates a received config arg
   */
  private def validate(config: SetupConfigArg): Validation = {

    // Checks a single setup config
    def validateConfig(sc: SetupConfig): Validation = {
      if (sc.configKey.prefix != HCDExample.prefix) {
        Invalid("Wrong prefix")
      } else {
        val missing = sc.missingKeys(HCDExample.rateKey)
        if (missing.nonEmpty)
          Invalid(s"Missing keys: ${missing.mkString(", ")}")
        else Valid
      }
    }

    val list = config.configs.map(validateConfig).filter(!_.isValid)
    if (list.nonEmpty) list.head else Valid
  }

  override protected def setup(locationsResolved: Boolean, configArg: SetupConfigArg,
                               replyTo: Option[ActorRef]): Validation = {
    val valid = validate(configArg)
    if (valid.isValid) {
      // Call a convenience method that will forward the config to the HCD based on the prefix
      distributeSetupConfigs(locationsResolved, configArg, None)

      // If a replyTo actor was given, reply with the command status
      if (replyTo.isDefined) {
        replyTo.get ! CommandStatus.Completed(configArg.info.runId)
      }
    }
    valid
  }
}

/**
 * Starts Hcd as a standalone application.
 */
object AssemblyExampleApp extends App {
  println("Starting Assembly1")
  LocationService.initInterface()
  val assemblyName = "assemblyExample"
  val className = "csw.examples.AssemblyExample"
  val componentId = ComponentId(assemblyName, ComponentType.Assembly)
  val targetHcdConnection = AkkaConnection(ComponentId(HCDExample.hcdName, ComponentType.HCD))
  val hcdConnections: Set[Connection] = Set(targetHcdConnection)
  val assemblyInfo = AssemblyInfo(assemblyName, "", className, RegisterOnly, Set(AkkaType), hcdConnections)
  val (supervisorSystem, supervisor) = Supervisor.create(assemblyInfo)

  // The code below shows how you could shut down the assembly
  if (false) {
    val system = ActorSystem("AssemblyExampleApp")
    import system.dispatcher
    system.scheduler.scheduleOnce(15.seconds) {
      Supervisor.haltComponent(supervisor)
      Await.ready(supervisorSystem.whenTerminated, 5.seconds)
      Await.ready(system.terminate(), 5.seconds)
      System.exit(0)
    }
  }
}
