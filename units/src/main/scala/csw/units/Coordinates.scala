package csw.units

import java.text.ParseException

import squants.AbstractQuantityNumeric
import squants.space._


case class Sign(sn: Int) {
  require(-1 <= sn && sn <= 1, "Invalid signum: %d".format(sn))
  def *(value: Double) = sn * value
  override def toString = if (sn == -1) "-" else ""
}

object Sign {
  def apply(s: String): Sign = s match {
    case null => Sign(1)
    case ""   => Sign(1)
    case "+"  => Sign(1)
    case "-"  => Sign(-1)
  }
  def apply[A](n: A)(implicit x: Numeric[A]): Sign = Sign(x.signum(n))
}

private object Sexigesimal {
  val Pat   = """([-+])?(\d\d?):(\d\d?):(\d\d?\.?\d*)""".r
  val Pat2   = """([-+])?(\d\d?):(\d\d?)""".r

  def parse[T](s: String, f: (Sign, Int, Int, Double) => T): Option[T] = s.trim match {
    case Pat(a, b, c, d) => Some(f(Sign(a), b.toInt, c.toInt, d.toFloat))
    case Pat2(a, b, c) => Some(f(Sign(a), b.toInt, c.toInt, 0.0))
    case _               => None
  }
}

object DMS {

  def apply(s: String): DMS = Sexigesimal.parse(s, DMS(_,_,_,_)).getOrElse(throw new ParseException(s, 0))

  def apply(deg: Double): DMS = {
    val sign = Sign(deg)
    val d = deg.abs % 360 // not normalized to positive; assumed to be in (-90, 90) if it's Dec
    val m = (deg.abs - deg.abs.intValue) * 60
    val s = (m - m.intValue) * 60
    DMS(sign, d.intValue, m.intValue, s)
  }

  // In some cases nn:mm:59.9999999999 becomes nn:mm:60.000 which isn't good. So we have to work around it.
  def format(sn: Sign, a: Int, b: Int, c: Double):String = (a, b, c) match {
    case (_, 60, _) => format(sn, a + 1, 59, c)
    case _ =>
      val s = "%s%d:%02d:%06.3f".format(sn, a, b, c)
      if (s.endsWith("60.000")) format(sn, a, b + 1, 0) else s
  }
}


case class DMS(sn: Sign, d: Int, m: Int, s: Double = 0.0) {
  require(d >= 0,  "Invalid DMS: %s".format(DMS.format(sn, d, m, s)))
  require(m >= 0 && m < 60, "Invalid DMS: %s".format(DMS.format(sn, d, m, s)))
  require(s >= 0 && s < 60, "Invalid DMS: %s".format(DMS.format(sn, d, m, s)))

  override def toString = DMS.format(sn, d, m, s)
  lazy val toDmsString = toString
  lazy val toDegrees = sn * (d + m / 60.0 + s / (60 * 60))
  lazy val degrees = Degrees(toDegrees)
}

//
// STRUCTURED HMS
//

object HourAngle extends AngleUnit {
  val symbol = "h"
  val multiplier = math.Pi / 12d
}

object HMS {

  def apply(s: String): HMS = Sexigesimal.parse(s, HMS(_,_,_,_)).getOrElse(throw new ParseException(s, 0))

  def apply(sn: Sign, h: Int, m: Int, s: Double): HMS = sn match {
    case Sign(-1) => HMS(-1 * HMS(h, m, s).toDegrees)
    case _        => HMS(h, m, s)
  }

  def apply(deg: Double): HMS = {
    val d = ((deg % 360) + 360) % 360
    val h = d / 15
    val m = (h - h.intValue) * 60
    val s = (m - m.intValue) * 60
    HMS(h.intValue, m.intValue, s)
  }

  // In some cases nn:mm:59.9999999999 becomes nn:mm:60.000 which isn't good. So we have to work around it.
  def format(a: Int, b: Int, c: Double): String = (a, b, c) match {
    case (24, _, _) => format(0, b, c)
    case (_, 60, _) => format(a + 1, 59, c)
    case _ =>
      val s = "%d:%02d:%06.3f".format(a, b, c)
      if (s.endsWith("60.000")) format(a, b + 1, 0) else s
  }
}

case class HMS(h: Int, m: Int, s: Double) {
  require(h >= 0 && h < 24, "Invalid HMS: %s".format(HMS.format(h, m, s)))
  require(m >= 0 && m < 60, "Invalid HMS: %s".format(HMS.format(h, m, s)))
  require(s >= 0 && s < 60, "Invalid HMS: %s".format(HMS.format(h, m, s)))
  override def toString = HMS.format(h, m, s)
  lazy val toHmsString = toString
  lazy val toDegrees = h * 15.0 + m / 4.0 + s / (4.0 * 60)
//  lazy val toHours = toDegrees/15.0
  lazy val toHours = h + m / 60.0 + s / (60 * 60)
  lazy val hours = HourAngle(toHours)
}


object Coordinates {
  implicit def stringToDms(s: String): DMS = DMS(s)
  implicit def stringToHms(s: String): HMS = HMS(s)
  implicit def angleToHms(a: Angle): HMS = HMS(a.toDegrees)
  implicit def angleToDms(a: Angle): DMS = DMS(a.toDegrees)

  implicit class CoordinatesConversions[A](n: A)(implicit num: Numeric[A]) {
    def hours = HourAngle(n)
  }
}
