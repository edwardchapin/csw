package org.tmt.csw.test.container2

import akka.actor.{ActorRef, Props, Actor, ActorLogging}
import org.tmt.csw.pkg.Container
import akka.pattern.ask
import akka.util.Timeout

/**
 */
class Container2Actor extends Actor with ActorLogging {
  import scala.concurrent.duration._
  val duration: FiniteDuration = 2.seconds
  implicit val timeout = Timeout(duration)
  implicit val dispatcher = context.system.dispatcher

  def receive: Receive = {
    case x => log.info(s"Received unknown message: $x")
  }
  //    val container = Container.create("Container-2")
  val container = context.system.actorOf(Props[Container], "Container-2")

  val hcd2aProps = Hcd2.props("HCD-2A", Set("config.tmt.tel.base.pos"))
  val hcd2bProps = Hcd2.props("HCD-2B", Set("config.tmt.tel.ao.pos.one"))

  for {
    hcd2a <- (container ? Container.CreateComponent(hcd2aProps, "HCD-2A")).mapTo[ActorRef]
    hcd2b <- (container ? Container.CreateComponent(hcd2bProps, "HCD-2B")).mapTo[ActorRef]
  } {
    log.info(s"Created HCD-2A: ${hcd2a.path}")
    log.info(s"Created HCD-2B: $hcd2b")
  }
}