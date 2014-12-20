package csw.services.event

import akka.actor.{ Props, ActorRef, ActorLogging, Actor }
import csw.util.cfg.Events.EventType
import org.hornetq.api.core.client._
import java.util.UUID

/**
 * Adds the ability to subscribe to events.
 * The subscribed actor wil receive Event messages for the given channel.
 */
trait EventSubscriber {
  this: Actor with ActorLogging ⇒

  // Connect to Hornetq server
  private val hq = connectToHornetQ(context.system)

  // Unique id for this subscriber
  private val subscriberId = UUID.randomUUID().toString

  // Unique queue name for this subscriber
  private def makeQueueName(channel: String): String = s"$channel-$subscriberId"

  // Use a worker class to process incoming messages rather than block the receiver thread
  private val worker = context.actorOf(Props(classOf[EventSubscriberWorker], self))

  // Called when a HornetQ message is received
  private val handler = new MessageHandler() {
    override def onMessage(message: ClientMessage): Unit = {
      worker ! message
    }
  }

  // Local object used to manage a subscription.
  // It creates a queue with a unique name for each channel.
  case class SubscriberInfo(channel: String) {
    val coreSession = hq.sf.createSession(false, false, false)
    val queueName = makeQueueName(channel)
    coreSession.createQueue(channel, queueName, /*, filter */ false)
    coreSession.close()

    val messageConsumer = hq.session.createConsumer(queueName, null, -1, -1, false)
    messageConsumer.setMessageHandler(handler)
  }

  // Maps channel (hornetq address) to SubscriberInfo
  private var map = Map[String, SubscriberInfo]()

  /**
   * Subscribes this actor to events with the given channels.
   *
   * @param channels the channel for the events you want to subscribe to.
   */
  def subscribe(channels: String*): Unit = {
    for (channel ← channels) {
      map += (channel -> SubscriberInfo(channel))
    }
  }

  /**
   * Unsubscribes this actor from events from the given channel.
   *
   * @param channels the top channels for the events you want to unsubscribe from.
   */
  def unsubscribe(channels: String*): Unit = {
    for {
      channel ← channels
      info ← map.get(channel)
    } {
      map -= channel
      info.messageConsumer.close()
      hq.session.deleteQueue(info.queueName)
    }
  }

  override def postStop(): Unit = hq.close()
}

// Worker class used to process incoming messages rather than block the receiver thread
// while unpacking the message
case class EventSubscriberWorker(subscriber: ActorRef) extends Actor with ActorLogging {
  override def receive: Receive = {
    case message: ClientMessage ⇒
      try {
        val ar = Array.ofDim[Byte](message.getBodySize)
        message.getBodyBuffer.readBytes(ar)
        subscriber ! EventType(ar)
      } catch {
        case ex: Throwable ⇒ log.error(ex, s"Error forwarding message to $subscriber: $message")
      }
  }
}