package csw.services.ls

import akka.http.scaladsl.model.Uri

/**
 * Stores information needed to register with the location service
 * @param serviceId the name and service type of the actor (used by other actors to search for it)
 * @param configPath an optional dot separated path expression referring to a hierarchy in a
 *                   Configuration object (Default: the entire configuration)
 * @param actorUri optional actor URI for the actor registering
 * @param httpUri optional HTTP URI for the service registering
 */
case class RegInfo(serviceId: ServiceId, configPath: Option[String] = None,
                   actorUri: Option[Uri] = None, httpUri: Option[Uri] = None) {
  import upickle.default._
  import RegInfo._
  lazy val json = write(ValueType(this))
}

object RegInfo {
  // This is just like RegInfo but with string values for easier JSON conversion
  object ValueType {
    def apply(regInfo: RegInfo): ValueType = ValueType(
      regInfo.serviceId.str, regInfo.configPath,
      regInfo.actorUri.map(_.toString()), regInfo.httpUri.map(_.toString()))
  }
  case class ValueType(serviceId: String, configPath: Option[String] = None,
                       actorUri: Option[String] = None, httpUri: Option[String] = None) {
    def toRegInfo: RegInfo = RegInfo(ServiceId(serviceId), configPath, actorUri.map(Uri(_)), httpUri.map(Uri(_)))
  }

  def fromJson(json: String): RegInfo = {
    import upickle.default._
    read[ValueType](json).toRegInfo
  }
}

