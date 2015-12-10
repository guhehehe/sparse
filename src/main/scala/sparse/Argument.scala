package sparse

import scala.util.{Success, Try}

object Value {
  def unapply(v: String): Option[String] = Option(v).flatMap {
    case v if v.length() > 0 & !v.startsWith("-") => Some(v)
    case _ => None
  }
}

sealed abstract class Argument(val name: String, val value: Option[String] = None) {
  def setValue(newValue: String): Argument
}

object PositionalArgument {
  def apply(index: Int, name: String, value: Option[String] = None): PositionalArgument = {
    new PositionalArgument(index, name, value)
  }

  def unapply(k: String): Option[String] = {
    Option(k).filter(k => k.length() > 0 & !k.startsWith("-"))
  }
}

object OptionalArgument {
  def apply(
      name: String,
      value: Option[String] = None,
      flag: Option[String] = None): OptionalArgument = {
    new OptionalArgument(name, value, flag)
  }

  def unapply(k: String): Option[(String, Boolean)] = Option(k).flatMap {
    case k if k.startsWith("--") => Some((k.stripPrefix("--"), false))
    case k if k.startsWith("-") => Some((k.stripPrefix("-"), true))
    case _ => None
  }
}

class PositionalArgument(
    val index: Int,
    name: String,
    value: Option[String] = None) extends Argument(name, value) {

  override def setValue(newValue: String) = PositionalArgument(index, name, Option(newValue))

  override def toString = s"PositionalArgument(index=$index, " +
      s"name=$name, value=${value.getOrElse("NULL")})"
}

class OptionalArgument(
    name: String,
    value: Option[String] = None,
    val flag: Option[String] = None) extends Argument(name, value) {

  def isSwitch = value match {
    case Some(v) => Try(v.toBoolean) match {
      case Success(_) => true
      case _ => false
    }
    case _ => false
  }

  def setFlag(flag: String) = {
    OptionalArgument(name, value, if (flag.length() > 0) Option(flag) else None)
  }

  override def setValue(newValue: String) = OptionalArgument(name, Option(newValue), flag)

  override def toString = s"PositionalArgument(name=$name, " +
      s"value=${value.getOrElse("NULL")}, flag=${flag.getOrElse("NULL")})"
}
