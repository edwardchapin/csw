package csw.services.ls

import akka.actor.{ ActorSystem, Extension, ExtensionKey }

object LocationServiceSettings extends ExtensionKey[LocationServiceSettings]

/**
 * The configuration settings for the location service
 */
case class LocationServiceSettings(system: ActorSystem) extends Extension {
  val etcdHostname: String = system.settings.config getString "csw.location-service.etcd.hostname"
  val etcdPort: Int = system.settings.config getInt "csw.location-service.etcd.port"
}

