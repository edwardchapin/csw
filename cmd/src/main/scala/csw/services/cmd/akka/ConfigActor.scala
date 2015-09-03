package csw.services.cmd.akka

import akka.actor.{ ActorLogging, Actor, ActorRef }
import csw.services.ls.OldLocationServiceActor.ServicesReady
import csw.services.ls.OldLocationServiceClientActor.{ Disconnected, Connected }
import csw.shared.{ CommandStatus, RunId }
import scala.util.Try
import csw.util.cfg.Configurations._
import akka.actor.Terminated

object ConfigActor {

  // -- Messages that operate on a running configuration --
  sealed trait ConfigMessage

  sealed trait ConfigControlMessage {
    val runId: RunId

    def withRunId(newRunId: RunId): ConfigControlMessage
  }

  /**
   * Message to cancel the running config with the given runId
   */
  case class ConfigCancel(runId: RunId) extends ConfigMessage with ConfigControlMessage {
    def withRunId(newRunId: RunId): ConfigControlMessage = ConfigCancel(newRunId)
  }

  /**
   * Message to abort the running config with the given runId
   */
  case class ConfigAbort(runId: RunId) extends ConfigMessage with ConfigControlMessage {
    def withRunId(newRunId: RunId): ConfigControlMessage = ConfigAbort(newRunId)
  }

  /**
   * Message to pause the running config with the given runId
   */
  case class ConfigPause(runId: RunId) extends ConfigMessage with ConfigControlMessage {
    def withRunId(newRunId: RunId): ConfigControlMessage = ConfigPause(newRunId)
  }

  /**
   * Message to resume the running config with the given runId
   */
  case class ConfigResume(runId: RunId) extends ConfigMessage with ConfigControlMessage {
    def withRunId(newRunId: RunId): ConfigControlMessage = ConfigResume(newRunId)
  }

  /**
   * Used to query the current state of a device. A config is passed in (the values are ignored)
   * and a reply will be sent containing the same config with the current values filled out.
   *
   * @param config used to specify the keys for the values that should be returned
   */
  case class ConfigGet(config: SetupConfigList) extends ConfigMessage

  /**
   * The response from a ConfigGet command
   * @param tryConfig if all the requested values could be retrieved, Success(config), otherwise Failure(ex)
   */
  case class ConfigResponse(tryConfig: Try[SetupConfigList])

  /**
   * Can be used to configure the system (for internal use)
   * @param config contains internal configuration values (to be defined)
   */
  case class ConfigPut(config: SetupConfigList) extends ConfigMessage

}

/**
 * Command service targets can extend this class, which defines
 * methods for implementing the standard configuration control messages.
 */
trait ConfigActor extends Actor with ActorLogging {

  import ConfigActor._
  import CommandQueueActor._

  /**
   * A reference to this actor is needed to report the status of commands
   */
  def commandStatusActor: ActorRef

  /**
   * Messages received in the normal state.
   */
  def receiveConfigs: Receive = {
    case s: SubmitWithRunId       ⇒ submit(s)
    case ConfigCancel(runId)      ⇒ cancel(runId)
    case ConfigAbort(runId)       ⇒ abort(runId)
    case ConfigPause(runId)       ⇒ pause(runId)
    case ConfigResume(runId)      ⇒ resume(runId)

    case ConfigGet(config)        ⇒ query(config, sender())
    case ConfigPut(config)        ⇒ internalConfig(config)

    case Connected(servicesReady) ⇒ connected(servicesReady)
    case Disconnected             ⇒ disconnected()

    // An actor was terminated (normal when done)
    case Terminated(actor)        ⇒ terminated(actor)
  }

  /**
   * Called when a configuration is submitted
   */
  def submit(submit: SubmitWithRunId): Unit

  /**
   * Report the command status to the command status actor.
   * All extending should call this to report the command status.
   * @param status the command status
   * @param submitter the (original) submitter of the command
   */
  def returnStatus(status: CommandStatus, submitter: ActorRef): Unit = {
    commandStatusActor ! CommandStatusActor.StatusUpdate(status, submitter)
  }

  /**
   * Work on the config matching the given runId should be paused
   */
  def pause(runId: RunId): Unit

  /**
   * Work on the config matching the given runId should be resumed
   */
  def resume(runId: RunId): Unit

  /**
   * Work on the config matching the given runId should be canceled
   */
  def cancel(runId: RunId): Unit

  /**
   * Work on the config matching the given runId should be aborted
   */
  def abort(runId: RunId): Unit

  /**
   * Query the current state of a device and reply to the sender with a ConfigResponse object.
   * A config is passed in (the values are ignored) and the reply will be sent containing the
   * same config with the current values filled out.
   *
   * @param config used to specify the keys for the values that should be returned
   * @param replyTo reply to this actor with the config response
   */
  def query(config: SetupConfigList, replyTo: ActorRef): Unit

  /**
   * Used to configure the system (for internal use)
   * @param config contains internal configuration values (to be defined)
   */
  def internalConfig(config: SetupConfigList): Unit = {
    // XXX TODO to be defined... (make abstract)
  }

  /**
   * Called when the services requested from the location service are all available.
   * @param ready contains a list describing the available services
   */
  def connected(ready: ServicesReady): Unit = {
    log.debug(s"Connected: services ready: ${ready.services}")
  }

  /**
   * Called when one or more of the services requested from the location service is no longer available.
   */
  def disconnected(): Unit = {
    log.debug("Disconnected")
  }

  /**
   * Called when a child (worker) actor terminates
   */
  def terminated(actorRef: ActorRef): Unit = {
    log.debug(s"Actor $actorRef terminated")
  }
}
