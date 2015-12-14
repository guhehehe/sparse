package sparse

private[sparse] object PositionalArg {
  def unapply(k: String): Option[String] = {
    if (!k.isEmpty & !k.startsWith("-")) {
      Some(k)
    } else {
      None
    }
  }
}

private[sparse] class PositionalArg(
    val index: Int,
    name: String,
    value: String = "",
    options: Set[String] = Set.empty,
    desc: String = "",
    valFormat: String = "") extends Argument(name, value, options, desc, valFormat) {

  override def setValue(value: String) = {
    if (value != this.value) {
      new PositionalArg(index, name, value, options, desc)
    } else {
      this
    }
  }

  override def toString = s"PositionalArgument(index=$index, name=$name, value=$value, " +
      s"options=$options, desc=$desc)"
}

