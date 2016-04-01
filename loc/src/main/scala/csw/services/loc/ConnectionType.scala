package csw.services.loc

import scala.util.{Failure, Success, Try}

/**
 * Connection type: Indicate if it is an http server or an akka actor.
 */
sealed trait ConnectionType {
  def name: String

  override def toString = name
}

object ConnectionType {

  /**
   * Type of a REST/HTTP based service
   */
  case object HttpType extends ConnectionType {
    val name = "http"
  }

  /**
   * Type of an Akka actor based service
   */
  case object AkkaType extends ConnectionType {
    val name = "akka"
  }

  /**
   * Exception throws for an unknown connection type
   */
  case class UnknownConnectionTypeException(message: String) extends Exception(message)

  /**
   * Gets a ConnectionType from the string value ("akka" or "http") or an UnknownConnectionTypeException
   */
  def apply(name: String): Try[ConnectionType] = name match {
    case "http" ⇒ Success(HttpType)
    case "akka" ⇒ Success(AkkaType)
    case x      ⇒ Failure(UnknownConnectionTypeException(x))
  }

}
