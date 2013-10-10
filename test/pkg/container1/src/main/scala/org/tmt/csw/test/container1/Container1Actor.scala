package org.tmt.csw.test.container1

import akka.actor._
import org.tmt.csw.pkg.{Assembly, Container}
import org.tmt.csw.cmd.core.Configuration
import akka.pattern.ask
import org.tmt.csw.cmd.akka.{CommandStatus, CommandServiceMessage}
import org.tmt.csw.cmd.spray.CommandServiceHttpServer
import akka.util.Timeout

object Container1Actor {
  val testConfig =
    """
      |      config {
      |        info {
      |          configId = 1000233
      |          obsId = TMT-2021A-C-2-1
      |        }
      |        tmt.tel.base.pos {
      |          posName = NGC738B
      |          c1 = "22:35:58.530"
      |          c2 = "33:57:55.40"
      |          equinox = J2000
      |        }
      |        tmt.tel.ao.pos.one {
      |          c1 = "22:356:01.066"
      |          c2 = "33:58:21.69"
      |          equinox = J2000
      |        }
      |      }
      |
    """.stripMargin
}

class Container1Actor extends Actor with ActorLogging {
  import Container1Actor._
  import scala.concurrent.duration._
  val duration: FiniteDuration = 5.seconds
  implicit val timeout = Timeout(duration)

  val config = Configuration(testConfig)
  implicit val dispatcher = context.system.dispatcher // XXX ???

  //    val container = Container.create("Container-1")
  val container = context.actorOf(Props[Container], "Container-1")

  val assembly1Props = Assembly1.props("Assembly-1")

  val hcd2aPath = ActorPath.fromString(Container1Settings(context.system).hcd2a)
  val hcd2bPath = ActorPath.fromString(Container1Settings(context.system).hcd2b)
  var registeredCount = 0

  // Receive actor messages
  def receive: Receive = {
    case status: CommandStatus => log.info(s"received command status: $status")
    case x => log.warning(s"received unknown message $x")
  }

  for {
    assembly1 <- (container ? Container.CreateComponent(assembly1Props, "Assembly-1")).mapTo[ActorRef]
    a <- assembly1 ? Assembly.AddComponentByPath(hcd2aPath)
    b <- assembly1 ? Assembly.AddComponentByPath(hcd2bPath)
  } {
      log.info(s"XXX a = $a, b = $b")
      // Send a test config
      assembly1 ! CommandServiceMessage.Submit(config)

      // Start a HTTP server with the REST interface
      val interface = Container1Settings(context.system).interface
      val port = Container1Settings(context.system).port
      val timeout = Container1Settings(context.system).timeout
      context.actorOf(CommandServiceHttpServer.props(assembly1, interface, port, timeout), "commandService")
  }
}
