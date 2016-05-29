package csw.util.config3

import scala.annotation.varargs
import scala.collection.JavaConverters._
import scala.collection.immutable.Vector
import scala.language.implicitConversions
import csw.util.config3.UnitsOfMeasure.Units

/**
  * The type of a value for an BooleanKey
  *
  * @param keyName the name of the key
  * @param value   the value for the key
  * @param units   the units of the value
  */
final case class BooleanItem(keyName: String, value: Vector[Boolean], units: Units) extends Item[Boolean, java.lang.Boolean] {
  /**
    * Java API
    *
    * @return the values as a Scala Vector
    */
  override def jvalue: Vector[java.lang.Boolean] = value.map(i ⇒ i: java.lang.Boolean)

  /**
    * Java API
    *
    * @return the values as a Java List
    */
  def jvalues: java.util.List[java.lang.Boolean] = jvalue.asJava

  /**
    * Java API
    *
    * @return the value at the given index
    */
  override def jget(index: Int): java.lang.Boolean = value(index)

  /**
    * Set the units of the value
    *
    * @param unitsIn the units to set
    * @return a copy of this item with the given units set
    */
  override def withUnits(unitsIn: Units) = copy(units = unitsIn)
}

/**
  * A key of Boolean values
  *
  * @param nameIn the name of the key
  */
final case class BooleanKey(nameIn: String) extends Key[Boolean, java.lang.Boolean](nameIn) {

  /**
    * Sets the values for the key
    *
    * @param v     the values
    * @param units the units of the values
    * @return a new item containing the key name, values and units
    */
  override def set(v: Vector[Boolean], units: Units) = BooleanItem(keyName, v, units)

  /**
    * Sets the values for the key using a variable number of arguments
    *
    * @param v the values
    * @return a new item containing the key name, values and no units
    */
  override def set(v: Boolean*) = BooleanItem(keyName, v.toVector, units = UnitsOfMeasure.NoUnits)

  /**
    * Java API to set the values for a key
    *
    * @param v     the values as a java list
    * @param units the units of the values
    * @return a new item containing the key name, values and units
    */
  def jset(v: Vector[java.lang.Boolean], units: Units) = BooleanItem(keyName, v.map(i ⇒ i: Boolean), units)

  /**
    * Java API to set the values for a key
    *
    * @param v     the values as a java list
    * @param units the units of the values
    * @return a new item containing the key name, values and units
    */
  def jset(v: java.util.List[java.lang.Boolean], units: Units) = jset(v.asScala.toVector, units)

  /**
    * Java API: Sets the values for the key using a variable number of arguments
    *
    * @param v the values
    * @return a new item containing the key name, values and no units
    */
  @varargs
  override def jset(v: java.lang.Boolean*) = BooleanItem(keyName, v.map(i ⇒ i: Boolean).toVector, units = UnitsOfMeasure.NoUnits)
}

