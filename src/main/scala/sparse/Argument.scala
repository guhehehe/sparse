package sparse

import scala.util.{Success, Try}

object Value {
  def unapply(v: String): Option[String] = Option(v).flatMap {
    case v if v.length() > 0 & !v.startsWith("-") => Some(v)
    case _ => None
  }
}

sealed abstract class Argument(
    val name: String,
    val value: Option[String] = None,
    val options: Set[String] = Set.empty,
    val desc: Option[String] = None) {

  value.foreach {
    case v if options.isEmpty | options(v) =>
    case v => {
      val optStr = options.map(s => s""""$s"""").mkString(", ")
      System.err.println( s"""`$name` must be chosen from {$optStr}, got "${value.orNull}"""")
      System.exit(1)
    }
  }

  def setValue(newValue: String): Argument
}

object PositionalArgument {
  def apply(
      index: Int,
      name: String,
      value: Option[String] = None,
      options: Set[String] = Set.empty,
      desc: Option[String] = None): PositionalArgument = {

    new PositionalArgument(index, name, value, options, desc)
  }

  def unapply(k: String): Option[String] = {
    Option(k).filter(k => k.length() > 0 & !k.startsWith("-"))
  }
}

object OptionalArgument {
  def apply(
      name: String,
      value: Option[String] = None,
      flag: Option[String] = None,
      options: Set[String] = Set.empty,
      desc: Option[String] = None): OptionalArgument = {
    new OptionalArgument(name, value, flag, options, desc)
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
    value: Option[String] = None,
    options: Set[String] = Set.empty,
    desc: Option[String] = None) extends Argument(name, value, options, desc) {

  override def setValue(newValue: String) = {
    PositionalArgument(index, name, Option(newValue), options, desc)
  }

  override def toString = s"PositionalArgument(index=$index, " +
      s"name=$name, value=${value.getOrElse("NULL")})"
}

class OptionalArgument(
    name: String,
    value: Option[String] = None,
    val flag: Option[String] = None,
    options: Set[String] = Set.empty,
    desc: Option[String] = None) extends Argument(name, value, options, desc) {

  def isSwitch = value match {
    case Some(v) => Try(v.toBoolean) match {
      case Success(_) => true
      case _ => false
    }
    case _ => false
  }

  def setFlag(flag: String) = {
    OptionalArgument(name, value, if (flag.length() > 0) Option(flag) else None, options, desc)
  }

  override def setValue(newValue: String) = {
    OptionalArgument(name, Option(newValue), flag, options, desc)
  }

  override def toString = s"PositionalArgument(name=$name, " +
      s"value=${value.getOrElse("NULL")}, flag=${flag.getOrElse("NULL")})"
}