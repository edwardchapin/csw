package csw.units

import com.typesafe.scalalogging.slf4j.LazyLogging
import org.scalatest._
import squants.space.AngleConversions._
import squants.space.LengthConversions._
import squants.space.{Microns, Nanometers, Degrees}
import Coordinates._

class UnitsTests extends FunSuite with LazyLogging with ShouldMatchers {
  val format = "%.2f"
  val tolerance = 0.0001
  def assertClose(d1: Double, d2: Double) = d1 should be (d2 +- tolerance)

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

    // "rad" is the base unit: angle values are stored in rad
    assert(ra.valueUnit.symbol == "rad")
  }

  test("Length test") {
    val d = 1.nm + 1.µm
    println(s"d = ${d.toString(Microns, "%.3f")}")
    assert(d.toString(Microns, "%.3f") == "1.001 µm")
    assertClose(d.toMicrons, 1.001)
  }
}
