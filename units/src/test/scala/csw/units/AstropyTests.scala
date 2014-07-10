package csw.units

import com.typesafe.scalalogging.slf4j.LazyLogging
import Coordinates._
import org.scalatest._

/**
 * Some tests ported from astropy
 */
class AstropyTests extends FunSuite with LazyLogging with ShouldMatchers {

  // coordinates

  val tolerance = 0.0001
  def assertClose(d1: Double, d2: Double) = d1 should be (d2 +- tolerance)

  test("negative zero dms") {
    // Test for DMS parser
    val a = "-00:00:10".degrees
    assertClose(a.toDegrees, -10.0 / 3600.0)
  }

  test("negative zero dm") {
    // Test for DM parser
    val a = "-00:10".degrees
    assertClose(a.toDegrees, -10.0 / 60.0)
  }

  test("zero hms") {
    // Test for HMS parser
    val a = "00:00:10".hours
    assertClose(a.toHours, 10.0 / 3600.0)
  }

  test("zero hm") {
    // Test for HM parser
    val a = "00:10".hours
    assertClose(a.toHours, 10.0 / 60.0)
  }
}
