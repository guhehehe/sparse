package sparse

import scala.util.{Success, Try}

private[sparse] object OptionalArg {
  def unapply(k: String): Option[(String, Boolean)] = Option(k).flatMap {
    case k if k.startsWith("--") => Some((k.stripPrefix("--"), false))
    case k if k.startsWith("-") => Some((k.stripPrefix("-"), true))
    case _ => None
  }
}

private[sparse] class OptionalArg(
    name: String,
    value: String = "",
    val flag: String = "",
    options: Set[String] = Set.empty,
    desc: String = "",
    valFormat: String = "") extends Argument(name, value, options, desc, valFormat) {

  private[this] def update(
      name: String = this.name,
      value: String = this.value,
      flag: String = this.flag,
      options: Set[String] = this.options,
      desc: String = this.desc,
      valFormat: String = this.valFormat): OptionalArg = {
    new OptionalArg(name, value, flag, options, desc, valFormat)
  }

  def isSwitch = Try(value.toBoolean) match {
    case Success(_) => true
    case _ => false
  }

  def setFlag(flag: String) = {
    if (!flag.isEmpty) {
      flag match {
        case OptionalArg(_, isFlag) if flag.isEmpty | isFlag => update(flag = flag.stripPrefix("-"))
        case unknown => throw new IllegalArgumentException(
          s"Can't handle flag: $unknown, make sure flag argument is prefixed by a single '-'."
        )
      }
    } else {
      this
    }
  }

  override def setValue(value: String) = {
    if (value != this.value) {
      update(value = value)
    } else {
      this
    }
  }

  override def toString = s"PositionalArgument(name=$name, value=$value, flag=$flag " +
      s"options=$options, desc=$desc)"
}