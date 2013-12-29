package org.tmt.csw.cmd.spray

import akka.actor.{ExtendedActorSystem, Extension, ExtensionKey}
import scala.concurrent.duration.{Duration, FiniteDuration, MILLISECONDS}

object CommandServiceTestSettings extends ExtensionKey[CommandServiceTestSettings]

/**
 * The test settings for the command service as an Akka extension:
 * - `interface`: the network interface the service gets bound to, e.g. `"localhost"`.
 * - `port`: the port the service gets bound to, e.g. `8080`.
 * - `timeout`: the amount of time to wait when polling for the command status
 */
class CommandServiceTestSettings(system: ExtendedActorSystem) extends Extension {

  /**
   * The network interface the command service service gets bound to, e.g. `"localhost"`.
   */
  val interface: String = system.settings.config getString "csw.testCommandService.interface"

  /**
   * The port the command service service gets bound to, e.g. `8080`.
   */
  val port: Int = system.settings.config getInt "csw.testCommandService.port"

  val timeout: FiniteDuration =
    Duration(system.settings.config getMilliseconds "csw.testCommandService.timeout", MILLISECONDS)
}
