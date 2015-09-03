package csw.services.ls

import akka.actor.ActorSystem
import com.tecsisa.etcd.EtcdClient
import com.typesafe.scalalogging.slf4j.Logger
import org.slf4j.LoggerFactory

import scala.concurrent.Future

/**
 * Location service based on CoreOs etcd (assumed to be running)
 */
object LocationService {
  val logger = Logger(LoggerFactory.getLogger("LocationService"))

  // Etcd directory used by the location service
  private val dir = "/loc"

  // Get the etcd key from the serviceId
  private def getKey(serviceId: String): String = s"$dir/$serviceId"

  private def getKey(serviceId: ServiceId): String = getKey(serviceId.str)
}

/**
 * Stores service location information in etcd and provides methods for setting and accessing it.
 */
case class LocationService(system: ActorSystem) {

  import LocationService._
  import system.dispatcher

  private val settings = LocationServiceSettings(system)
  private val etcdClient = EtcdClient(s"http://${settings.etcdHostname}:${settings.etcdPort}")

  /**
   * Register with the location service.
   *
   * @param regInfo contains the information to be stored (key is the ServiceId)
   */
  def register(regInfo: RegInfo): Future[Unit] = {
    val key = getKey(regInfo.serviceId)
    etcdClient.setKey(key, regInfo.json).map(_ ⇒ ())
  }

  /**
   * Convenience method that gets the location service information from the service, given the serviceId.
   * @param serviceId name and service type to look for
   * @param wait if true and the service was not found, wait for it to register
   * @return a future containing the service information, if found
   */
  def resolve(serviceId: ServiceId, wait: Boolean = true): Future[Option[RegInfo]] = {
    val key = getKey(serviceId)
    etcdClient.getKeyAndWait(key, wait).map(_.node.value.map(RegInfo.fromJson)).flatMap {
      case s @ Some(regInfo) ⇒ Future.successful(s)
      case None if !wait     ⇒ Future.successful(None)
      case None              ⇒ resolve(serviceId, wait) // timed out? try again
    }
  }

  /**
   * Convenience method  to search for services matching the given name and/or service type.
   * @param name optional service name (default: any)
   * @param serviceType optional service type: Defaults to any type
   */
  def browse(name: Option[String], serviceType: Option[ServiceType]): Future[List[RegInfo]] = {
    // Returns true if the given query matches the given serviceId
    def matchService(regInfo: RegInfo): Boolean = {
      (name, serviceType) match {
        case (None, None)           ⇒ true
        case (Some(s), None)        ⇒ s == regInfo.serviceId.name
        case (None, Some(sType))    ⇒ sType == regInfo.serviceId.serviceType
        case (Some(s), Some(sType)) ⇒ s == regInfo.serviceId.name && sType == regInfo.serviceId.serviceType
      }
    }

    for {
      r ← etcdClient.lsDir(dir)
    } yield {
      val it = for {
        nodes ← r.node.nodes.toSeq
        node ← nodes
        regInfo ← node.value.toSeq.map(RegInfo.fromJson) if matchService(regInfo)
      } yield regInfo
      it.toList
    }
  }

}

