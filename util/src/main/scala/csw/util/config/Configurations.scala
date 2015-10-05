package csw.util.config

import java.util.UUID

import scala.language.implicitConversions

/**
 * Defines the different types of configurations: setup, observe, wait, ...
 */
object Configurations {

  /**
   * Contains implicit def for getting the subsystem and prefix from a config key string
   */
  object ConfigKey {
    // Path separator
    val SEPARATOR = '.'

    /**
     * Creates a ConfigKey from the given string
     */
    implicit def apply(prefix: String): ConfigKey = ConfigKey(subsystem(prefix), prefix)

    // Returns the subsystem for the given string (up to first SEPARATOR), or Subsystem.BAD, if no SEPARATOR was found
    private def subsystem(keyText: String): Subsystem =
      Subsystem.lookup(keyText.splitAt(keyText.indexOf(SEPARATOR))._1).getOrElse(Subsystem.BAD)
  }

  /**
   * Convenience class for getting the subsystem and prefix from a config key string
   */
  case class ConfigKey(subsystem: Subsystem, prefix: String) {
    override def toString = "[" + subsystem + ", " + prefix + "]"
  }

  /**
   * Base trait for configurations: Defines the subsystem, prefix and a method to get the value for a key.
   * The config key is based on a string like subsystem.x.y.z, where the prefix is then subsystem.x.y.
   */
  sealed trait ConfigType extends KvsType {
    /**
     * Returns an object providing the subsystem and prefix for the config
     */
    def configKey: ConfigKey

    /**
     * The subsystem for the config
     */
    def subsystem: Subsystem = configKey.subsystem

    /**
     * The prefix for the config
     */
    def prefix: String = configKey.prefix

    def doToString(kind: String) =
      kind + "[" + subsystem + ", " + prefix + "] " + data.toString
  }

  /**
   * Marker trait for sequence configurations
   */
  sealed trait SequenceConfig extends ConfigType

  /**
   * Marker trait for control configurations
   */
  sealed trait ControlConfig extends ConfigType

  /**
   * Defines a setup configuration, which is a config key plus a set of key/value pairs
   * @param configKey the key for the configuration, containing subsystem and prefix
   * @param data the typed key/value pairs
   */
  case class SetupConfig(configKey: ConfigKey, data: ConfigData = ConfigData())
      extends SequenceConfig with ControlConfig {

    def set(key: Key)(value: key.Value): SetupConfig = SetupConfig(configKey, data.set(key)(value))

    def remove(key: Key): SetupConfig = SetupConfig(configKey, data.remove(key))

    override def toString = doToString("SC")
  }

  //  object SetupConfig {
  //    import scala.pickling.Defaults._
  //    import scala.pickling.binary._
  //
  //    /**
  //     * Defines the automatic conversion to a ByteString and back again.
  //     */
  //    implicit val byteStringFormatter = new ByteStringFormatter[SetupConfig] {
  //      def serialize(t: SetupConfig): ByteString = {
  //        ByteString(t.pickle.value)
  //      }
  //
  //      def deserialize(bs: ByteString): SetupConfig = {
  //        val ar = Array.ofDim[Byte](bs.length)
  //        bs.asByteBuffer.get(ar)
  //        ar.unpickle[SetupConfig]
  //      }
  //    }
  //  }

  /**
   * Defines an observe configuration, which is a config key plus a set of key/value pairs
   * @param configKey the key for the configuration, containing subsystem and prefix
   * @param data the typed key/value pairs
   */
  case class ObserveConfig(configKey: ConfigKey, data: ConfigData = ConfigData())
      extends SequenceConfig with ControlConfig {
    def set(key: Key)(value: key.Value): ObserveConfig = ObserveConfig(configKey, data.set(key)(value))

    def remove(key: Key): ObserveConfig = ObserveConfig(configKey, data.remove(key))

    override def toString = doToString("OC")
  }

  //  object ObserveConfig {
  //    import scala.pickling.Defaults._
  //    import scala.pickling.binary._
  //
  //    /**
  //     * Defines the automatic conversion to a ByteString and back again.
  //     */
  //    implicit val byteStringFormatter = new ByteStringFormatter[ObserveConfig] {
  //      def serialize(t: ObserveConfig): ByteString = {
  //        ByteString(t.pickle.value)
  //      }
  //
  //      def deserialize(bs: ByteString): ObserveConfig = {
  //        val ar = Array.ofDim[Byte](bs.length)
  //        bs.asByteBuffer.get(ar)
  //        ar.unpickle[ObserveConfig]
  //      }
  //    }
  //  }

  /**
   * Defines a wait configuration
   * @param configKey the key for the configuration, containing subsystem and prefix
   * @param data the typed key/value pairs
   */
  case class WaitConfig(configKey: ConfigKey, data: ConfigData = ConfigData())
      extends SequenceConfig {
    def set(key: Key)(value: key.Value): WaitConfig = WaitConfig(configKey, data.set(key)(value))

    def remove(key: Key): WaitConfig = WaitConfig(configKey, data.remove(key))

    override def toString = doToString("WAIT")
  }

  /**
   * Filters for sequences of configs
   */
  object ConfigFilters {
    // A filter type for various ConfigData
    type ConfigFilter[A] = A ⇒ Boolean

    def prefixes(configs: Seq[ConfigType]): Set[String] = configs.map(_.prefix).toSet

    def onlySetupConfigs(configs: Seq[ConfigType]): Seq[SetupConfig] = configs.collect { case ct: SetupConfig ⇒ ct }

    def onlyObserveConfigs(configs: Seq[ConfigType]): Seq[ObserveConfig] = configs.collect { case ct: ObserveConfig ⇒ ct }

    def onlyWaitConfigs(configs: Seq[ConfigType]): Seq[WaitConfig] = configs.collect { case ct: WaitConfig ⇒ ct }

    private val prefixStartsWithFilter: String ⇒ ConfigFilter[ConfigType] = query ⇒ sc ⇒ sc.prefix.startsWith(query)
    private val prefixContainsFilter: String ⇒ ConfigFilter[ConfigType] = query ⇒ sc ⇒ sc.prefix.contains(query)

    def prefixStartsWith(query: String, configs: Seq[ConfigType]): Seq[ConfigType] = configs.filter(prefixStartsWithFilter(query))

    def prefixContains(query: String, configs: Seq[ConfigType]): Seq[ConfigType] = configs.filter(prefixContainsFilter(query))
  }

  /**
   * This will include information related to the observation that is related to a configuration.
   * This will grow and develop.
   *
   * @param obsId the observation ID
   */
  case class ConfigInfo(obsId: ObsID) {
    /**
     * Unique ID for this configuration
     */
    val runId: UUID = UUID.randomUUID()
  }

  object ConfigInfo {
    implicit def apply(obsId: String): ConfigInfo = ConfigInfo(ObsID(obsId))
  }

  /**
   * A ConfigArg is what is placed in a Submit message in the Command Service queue.
   * It can be one or more SetupConfigs, one or more ObserveConfigs or a WaitConfig
   * Each ConfigArg includes a ConfigInfo which will contain information about the executing
   * observation.
   */
  sealed trait ConfigArg extends Serializable {
    def info: ConfigInfo
  }

  /**
   * Marker trait for sequence config args
   */
  sealed trait SequenceConfigArg extends ConfigArg

  /**
   * Marker trait for control config args
   */
  sealed trait ControlConfigArg extends ConfigArg

  final case class SetupConfigArg(info: ConfigInfo, configs: Seq[SetupConfig])
    extends SequenceConfigArg with ControlConfigArg

  object SetupConfigArg {
    def apply(configs: SetupConfig*)(implicit info: ConfigInfo): SetupConfigArg = SetupConfigArg(info, configs.toSeq)
  }

  final case class ObserveConfigArg(info: ConfigInfo, configs: Seq[ObserveConfig])
    extends SequenceConfigArg with ControlConfigArg

  object ObserveConfigArg {
    def apply(configs: ObserveConfig*)(implicit info: ConfigInfo): ObserveConfigArg = ObserveConfigArg(info, configs.toSeq)
  }

  final case class WaitConfigArg(info: ConfigInfo, config: WaitConfig)
    extends SequenceConfigArg

  object WaitConfigArg {
    def apply(config: WaitConfig)(implicit info: ConfigInfo): WaitConfigArg = WaitConfigArg(info, config)
  }

  type ConfigArgList = Seq[SequenceConfig]

  //  // For getting device configuration
  //  // XXX Allan: Should be part of command service actor messages
  //  final case class ConfigQuery(configs: Seq[SetupConfig])
  //
  //  sealed trait ConfigQueryResponse
  //
  //  final case class QuerySuccess(configs: Seq[SetupConfig]) extends ConfigQueryResponse
  //
  //  final case class QueryFailure(reason: String) extends ConfigQueryResponse

}