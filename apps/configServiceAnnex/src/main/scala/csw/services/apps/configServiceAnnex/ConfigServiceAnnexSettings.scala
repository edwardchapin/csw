package csw.services.apps.configServiceAnnex

/**
 * Config Service Annex settings based on the Akka application.conf file
 * (under resources in this module)
 */

import java.io.File

import akka.actor.{ ExtendedActorSystem, Extension, ExtensionId, ExtensionIdProvider }
import akka.util.Timeout
import com.typesafe.config.Config

import scala.concurrent.duration.Duration
import scala.concurrent.duration._

object ConfigServiceAnnexSettings extends ExtensionId[ConfigServiceAnnexSettings] with ExtensionIdProvider {
  override def lookup(): ConfigServiceAnnexSettings.type = ConfigServiceAnnexSettings

  override def createExtension(system: ExtendedActorSystem): ConfigServiceAnnexSettings = new ConfigServiceAnnexSettings(system.settings.config)
}

class ConfigServiceAnnexSettings(config: Config) extends Extension {
  val interface = config.getString("csw.config-service-annex.interface")
  val port = config.getInt("csw.config-service-annex.port")
  val dir = new File(subst(config.getString("csw.config-service-annex.dir")))
  val chunkSize = config.getInt("csw.config-service-annex.chunkSize")
  val timeout = Timeout(config.getDuration("csw.config-service-annex.timeout", MILLISECONDS), MILLISECONDS)

  // Do any required substitution on the setting values
  def subst(s: String): String = {
    s.replaceFirst("~", System.getProperty("user.home"))
  }
}

