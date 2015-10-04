package csw.services.ccs

import akka.actor.{ ActorLogging, Actor }
import csw.util.config.Configurations._

import scala.collection.immutable.Queue
import scala.concurrent.duration.{ Duration, FiniteDuration }

/**
 * Command service controller
 */
object HcdController {

  /**
   * The type of the queue of incoming configs
   */
  type QueueType = Queue[SetupConfig]

  /**
   * Base trait of all received messages
   */
  sealed trait HcdControllerMessage

  /**
   * Tells the controller to check its inputs and update its outputs
   */
  case object Process extends HcdControllerMessage
}

/**
 * Base trait of command service controller actors.
 * This is the main actor that accepts setup or observe configurations to execute.
 */
trait HcdController extends Actor with ActorLogging {

  import HcdController._
  import context.dispatcher

  /**
   * The queue of incoming configs
   */
  protected var queue = Queue.empty[SetupConfig]

//  /**
//   * Removes and returns the next SetupConfig from the queue, or None if the queue is empty
//   */
//  protected def nextConfig: Option[SetupConfig] = {
//    if (queue.nonEmpty) {
//      val (config, q) = queue.dequeue
//      queue = q
//      Some(config)
//    } else None
//  }
//
//  /**
//   * Returns the next SetupConfig in the queue without removing it, or None if the queue is empty
//   */
//  protected def peekConfig: Option[SetupConfig] = {
//    queue.headOption
//  }

  /**
   * Periodic method to be implemented by the HCD.
   * This method can use the nextConfig method to pop the next config from the queue
   * and the key/value store API (XXX TODO) to set the demand and current values.
   */
  protected def process(): Unit

  /**
   * The controller update rate: The controller inputs and outputs are processed at this rate
   */
  def rate: FiniteDuration



  // Sends the Update message at the specified rate
  context.system.scheduler.schedule(Duration.Zero, rate, self, Process)

  def receive: Receive = {
    case Process ⇒
      process()

    case config: SetupConfig ⇒
      queue = queue.enqueue(config)

    case x ⇒ log.error(s"Received unexpected message $x")
  }
}
