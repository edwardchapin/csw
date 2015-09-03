package csw.services.ls

import akka.actor._
import akka.http.scaladsl.model.Uri

import scala.concurrent.Future
import scala.util.{ Try, Failure, Success }

/**
 * A worker actor that does the job of requesting information about a list of services
 * from the location service and then watching them in case one of them terminates.
 * Once all the services are available, a Connected message is sent to the parent actor.
 * If any of the services terminate, a disconnected message is sent to the parent.
 */
object LocationServiceClientActor {

  /**
   * Type of messages sent to the requester
   */
  sealed trait LocationServiceClientMessage

  /**
   * Sent to parent when all services are ready
   * @param services list containing information about the services
   */
  case class Connected(services: List[RegInfo]) extends LocationServiceClientMessage

  /**
   * Sent to parent when one of the actors for a requested service terminates.
   */
  case object Disconnected extends LocationServiceClientMessage

  /**
   * Used to create this actor
   * @param serviceIds describes the services to check for
   */
  def props(serviceIds: List[ServiceId]): Props = Props(classOf[LocationServiceClientActor], serviceIds)
}

class LocationServiceClientActor(serviceIds: List[ServiceId]) extends Actor with ActorLogging {
  import LocationServiceClientActor._
  import context.dispatcher

  val locationService = LocationService(context.system)

  // Request the services from the location service
  requestServices()

  // Start out in the waiting state
  override def receive: Receive = waitingForServices

  // Initial state until we get a list of running services
  def waitingForServices: Receive = {
    //    case s @ ServicesReady(services) ⇒
    //      log.debug(s"All requested services are ready: $services")
    //      for (actorRef ← services.map(_.actorRefOpt).flatten) context.watch(actorRef)
    //      context.parent ! Connected(s)
    //      context.become(ready(services))

    case Terminated(actorRef) ⇒

    case x ⇒
      log.error(s"Unexpected message from ${sender()} while waiting for services: $x")
  }

  //  // Messages received in the ready state.
  //  def ready(services: List[LocationServiceInfo]): Receive = {
  //
  //    // If a target actor died, go back and wait for it (and any others that are needed) to restart
  //    case Terminated(actorRef) ⇒
  //      log.debug(s"Received terminated message for required service $actorRef: Waiting for it to come back.")
  //      requestServices()
  //      context.parent ! Disconnected
  //      context.become(waitingForServices)
  //
  //    case x ⇒ log.error(s"Unexpected message from ${sender()}: $x")
  //  }


  // Watch the actor with the given URI and if it dies, send a Disconnected message to the parent
   private def watchServiceActor(uri: Uri): Unit = {

  }

  // Lookup all the requested serviceIds and notify the parent when found
  private def requestServices(): Unit = {
    for(list <- Future.sequence(serviceIds.map(locationService.resolve(_)))) {
      if (list.contains(None)) {
        log.info(s"Not all services found, retrying")
        requestServices()
      } else {
        log.info(s"All services found")
        val regInfoList = list.flatten
        context.parent ! Connected(regInfoList)
        regInfoList.flatMap(_.actorUri).foreach(watchServiceActor)
      }
    }
  }
}
