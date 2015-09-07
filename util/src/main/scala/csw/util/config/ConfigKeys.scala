package csw.util.config

import csw.util.config.ConfigKeys._
import csw.util.config.UnitsOfMeasure.Units

object ConfigKeys {

  trait IntValued extends Key {
    type Value = Int
  }

  trait IntSeqValued extends Key {
    type Value = Seq[Int]
  }

  trait DoubleValued extends Key {
    type Value = Double
  }

  trait DoubleSeqValued extends Key {
    type Value = Seq[Double]
  }

  trait StringValued extends Key {
    type Value = String
  }

  trait StringSeqValued extends Key {
    type Value = Seq[String]
  }

  trait UnitsValued extends Key {
    type Value = Units
  }

  sealed abstract class ExposureClass(val description: String) extends Ordered[ExposureClass] {
    override def toString = description
    def compare(that: ExposureClass) = description.compare(that.description)
  }
  case object SCIENCE extends ExposureClass("science")
  case object NIGHTTIME_CALIBRATION extends ExposureClass("nighttime-calibration")
  case object DAYTIME_CALIBRATION extends ExposureClass("daytime-calibration")
  case object ACQUISITION extends ExposureClass("acquisition")

  trait ExposureClassValued extends Key {
    type Value = ExposureClass
  }

  sealed abstract class ExposureType(val description: String) extends Ordered[ExposureType] {
    override def toString = description
    def compare(that: ExposureType) = description.compare(that.description)
  }
  case object FLAT extends ExposureType("flat")
  case object ARC extends ExposureType("arc")
  case object BIAS extends ExposureType("bias")
  case object OBSERVE extends ExposureType("observe")

  trait ExposureTypeValued extends Key {
    type Value = ExposureType
  }

  sealed abstract class CloudCoverType(val description: String, val percentage: Int) extends Ordered[CloudCoverType] {
    override def toString = description
    def compare(that: CloudCoverType) = percentage.compare(that.percentage)
  }
  case object PERCENT_20 extends CloudCoverType("20%", 20)
  case object PERCENT_50 extends CloudCoverType("50%/Clear", 50)
  case object PERCENT_70 extends CloudCoverType("70%/Cirrus", 70)
  case object PERCENT_80 extends CloudCoverType("80%/Cloudy", 80)
  case object PERCENT_90 extends CloudCoverType("90%", 90)
  case object ANY extends CloudCoverType("Any", 100)

  trait CloudCoverValued extends Key {
    type Value = CloudCoverType
  }

}

object StandardKeys {

  // Common keys
  val UNITS = "units"

  val units = new Key(UNITS) with UnitsValued

  // Setup keys
  val POSITION = "position"
  val CLOUDCOVER = "cloudCover"

  val position = new Key(POSITION) with StringValued
  val cloudCover = new Key(CLOUDCOVER) with CloudCoverValued

  // ObserveConfig
  val EXPOSURETIME = "exposureTime"
  val EXPOSURETYPE = "exposureType"
  val EXPOSURECLASS = "exposureClass"
  val REPEATS = "repeats"

  val exposureTime = new Key(EXPOSURETIME) with DoubleValued

  val exposureType = new Key(EXPOSURETYPE) with ExposureTypeValued
  val exposureClass = new Key(EXPOSURECLASS) with ExposureClassValued
  val repeats = new Key(REPEATS) with IntValued
}
