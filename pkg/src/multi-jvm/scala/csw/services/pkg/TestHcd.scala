package csw.services.pkg

import akka.actor.Props
import csw.services.ccs.akka._
import csw.services.ls.LocationServiceActor
import LocationServiceActor._

object TestHcd {
  def props(name: String, configPath: String): Props = Props(classOf[TestHcd], name, configPath)
}

case class TestHcd(name: String, configPath: String) extends Hcd
  with CommandServiceActor with OneAtATimeCommandQueueController with LifecycleHandler {

  override val configActor = context.actorOf(TestConfigActor.props(commandStatusActor, 3), name)

  override def receive: Receive = receiveCommands orElse receiveLifecycleCommands

  val serviceId = ServiceId(name, ServiceType.HCD)
}
