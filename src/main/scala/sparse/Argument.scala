package sparse

private[sparse] object Value {
  def unapply(v: String): Option[String] = Option(v).flatMap {
    case v if v.nonEmpty && !v.startsWith("-") => Some(v)
    case _ => None
  }
}

private[sparse] abstract class Argument(
    val name: String,
    val value: String = "",
    val options: Set[String] = Set.empty,
    val desc: String = "",
    val valFormat: String = "") {

  // check if $value is in $options
  if (options.nonEmpty && value.nonEmpty && !options(value)) {
    val optStr = options.map(s => s""""$s"""").mkString(", ")
    throw new UnknownArgException(s"""`$name` must be chosen from {$optStr}, got "$value"""")
  }

  def setValue(newValue: String): Argument
}
