package csw.util.config

import csw.util.config.UnitsOfMeasure.{NoUnits, Units}
import spray.json.DefaultJsonProtocol

import scala.collection.immutable.Vector
import scala.language.implicitConversions

/**
 * A Scala equivalent of a 2d array of Ints
 */
case class IntMatrix(value: Array[Array[Int]]) {
  import ArrayAndMatrixEquality._

  override def toString = (for (l <- value) yield l.mkString("(", ",", ")")).mkString("(", ",", ")")

  def apply(row: Int, col: Int) = value(row)(col)

  override def canEqual(other: Any) = other.isInstanceOf[IntMatrix]

  override def equals(other: Any) = other match {
    case that: IntMatrix =>
      this.canEqual(that) && deepMatrixValueEquals(this.value, that.value)
    case _ => false
  }
}
case object IntMatrix extends DefaultJsonProtocol {
  implicit def format = jsonFormat1(IntMatrix.apply)

  implicit def create(value: Array[Array[Int]]):IntMatrix = IntMatrix(value)
}

/**
 * The type of a head for an IntMatrixKey: One or more 2d arrays (implemented as IntMatrix)
 *
 * @param keyName the name of the key
 * @param values   the head for the key
 * @param units   the units of the head
 */
final case class IntMatrixItem(keyName: String, values: Vector[IntMatrix], units: Units) extends Item[IntMatrix] {
  override def withUnits(unitsIn: Units) = copy(units = unitsIn)
}

/**
 * A key for IntMatrix values
 *
 * @param nameIn the name of the key
 */
final case class IntMatrixKey(nameIn: String) extends Key[IntMatrix, IntMatrixItem](nameIn) {

  override def set(v: Vector[IntMatrix], units: Units = NoUnits) = IntMatrixItem(keyName, v, units)

  override def set(v: IntMatrix*) = IntMatrixItem(keyName, v.toVector, units = UnitsOfMeasure.NoUnits)
}

