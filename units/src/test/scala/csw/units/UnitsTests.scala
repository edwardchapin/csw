package csw.units

import com.typesafe.scalalogging.slf4j.LazyLogging
import org.scalatest.FunSuite
import squants.space.AngleConversions._
import squants.space.Degrees
import Coordinates._

class UnitsTests extends FunSuite with LazyLogging {
  val format = "%.2f"

  test("Basic test") {
    val ra = 12.5.hours
    val dec = 10.2.degrees
    val dist = 2.5.hours
    val x = ra - dist

    assert(ra.toHours == 12.5)
    assert(ra.toDegrees == 12.5*15)
    assert(x.toHours == 10)
    assert(s"${x.toString(HourAngle, format)}" == "10.00 h")
  }

  test("HMS and DMS") {
    val ra = "12:30:00.0".hours
    val dec = "10:12:00.0".degrees
    println(s"ra = ${ra.toString(HourAngle, format)}, dec = ${dec.toString(Degrees, format)}")
    assert(ra.toDegrees/15 == 12.5)
    assert(ra.toHours == 12.5)
    assert(dec.toDegrees == 10.2)
    println(s"ra = ${ra.toHmsString}, dec = ${dec.toDmsString}")
    assert(ra.toHmsString == "12:30:00.000")
    assert(dec.toDmsString == "10:12:00.000")
  }


}
