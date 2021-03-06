package csw.services.pkg

import csw.util.config.Configurations.{SetupConfig, SetupConfigArg}
import csw.util.config.StringKey

// XXX TODO: This class is left over from previous versions
object TestConfig {

  val obsId = "TMT-2021A-C-2-1"

  val posName = StringKey("posName")
  val c1 = StringKey("c1")
  val c2 = StringKey("c2")
  val equinox = StringKey("equinox")

  // Configs to use for testing
  val testConfig1: SetupConfig = SetupConfig("tcs.base.pos").madd(
    posName.set("NGC738B"),
    c1.set("22:35:58.530"),
    c2.set("33:57:55.40"),
    equinox.set("J2000"))

  val testConfig2: SetupConfig = SetupConfig("tcs.ao.pos.one")
    .add(posName.set("NGC738B"))
    .add(c1.set("22:36:01.066"))
    .add(c2.set("33:58:21.69"))
    .add(equinox.set("J2000"))

  val testConfigArg = SetupConfigArg("obs0001", testConfig1, testConfig2)
}

