package csw.services.ccs

import akka.actor.{ Actor, Props, ActorRef }
import akka.util.Timeout
import csw.services.kvs.Subscriber
import csw.shared.cmd.{ RunId, CommandStatus }
import csw.util.cfg.Configurations.{ SetupConfig, StateVariable }
import csw.util.cfg.Configurations.StateVariable.Matcher
import scala.concurrent.duration._

// Needed for Subscriber below
import csw.services.kvs.Implicits.setupConfigKvsFormatter

object StateMatcherActor {

  /**
   * Props used to create the actor.
   *
   * @param demands the target states that will be compared to their current states
   * @param replyTo the actor to reply to
   * @param runId the runId to use in the reply
   * @param timeout the amount of time to wait for a match before giving up and replying with a Timeout message
   * @param matcher the function used to compare the demand and current states
   */
  def props(demands: List[SetupConfig], replyTo: ActorRef, runId: RunId = RunId(),
            timeout: Timeout = Timeout(10.seconds),
            matcher: Matcher = StateVariable.defaultMatcher): Props =
    Props(classOf[StateMatcherActor], demands, replyTo, runId, timeout, matcher)
}

/**
 * Subscribes to the current values for the given demand values and notifies the
 * replyTo actor with the command status when they all match the respective demand states,
 * or with an error status message if the given timeout expires.
 *
 * See props for a description of the arguments.
 */
class StateMatcherActor(demands: List[SetupConfig], replyTo: ActorRef, runId: RunId,
                        timeout: Timeout, matcher: Matcher)
    extends Subscriber[SetupConfig] {

  import context.dispatcher
  context.become(waiting(Set[SetupConfig]()))
  val keys = demands.map(_.prefix)
  log.info(s"Subscribing to ${keys.mkString(", ")}")
  subscribe(keys: _*)
  val timer = context.system.scheduler.scheduleOnce(timeout.duration, self, timeout)

  override def receive: Receive = Actor.emptyBehavior

  // Waiting for all variables to match, which is the case when the results set contains
  // a matching current state for each demand state
  def waiting(results: Set[SetupConfig]): Receive = {
    case current: SetupConfig ⇒
      log.info(s"received current state: $current")
      demands.find(_.prefix == current.prefix).foreach { demand ⇒
        if (matcher(demand, current)) {
          val set = results + current
          if (set.size == demands.size) {
            timer.cancel()
            replyTo ! CommandStatus.Completed(runId)
            context.stop(self)
          } else context.become(waiting(set))
        }
      }

    case `timeout` ⇒
      log.info(s"received timeout")
      replyTo ! CommandStatus.Error(runId, "Command timed out")
      context.stop(self)

    case x ⇒ log.error(s"Unexpected message $x")
  }
}
