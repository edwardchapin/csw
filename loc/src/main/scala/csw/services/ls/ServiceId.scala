package csw.services.ls

object ServiceId {
  /**
   * Initialize from string as returned from asString method
   */
  def apply(s: String): ServiceId = {
    s.splitAt(s.lastIndexOf('-')) match {
      case (x, name) if x.isEmpty ⇒ ServiceId(name, ServiceType.Unknown)
      case (name, str)            ⇒ ServiceId(name, ServiceType(str.substring(1)))
    }
  }
}

/**
 * Used to identify a service
 * @param name the service name
 * @param serviceType HCD, Assembly, Service
 */
case class ServiceId(name: String, serviceType: ServiceType) {

  /**
   * A string representation of a ServiceId that can be used to recreate the instance
   */
  lazy val str = s"$name-$serviceType"
}
