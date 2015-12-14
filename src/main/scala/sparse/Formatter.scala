package sparse

class Formatter(
    val progName: String,
    val desc: String,
    val posArgs: Iterable[PositionalArg],
    val optArgs: Iterable[OptionalArg]) {

  val defaultValueFmt = "<value>"
  val defaultPrecedingIndent = " " * 2
  val defaultDescIndent = " " * 8
  val defaultMaxCol = 79

  private[this] def generateOptArgFmt(arg: OptionalArg) = {
    val name = arg.name
    val flag = arg.flag
    val fmt = if (!arg.options.isEmpty) {
      s"{${arg.options.mkString("|")}}"
    } else if (!arg.valFormat.isEmpty) {
      arg.valFormat
    } else {
      defaultValueFmt
    }
    if (arg.isSwitch) {
      val partial = s"--$name"
      if (flag.isEmpty) partial else s"$partial|-$flag"
    } else {
      val partial = s"--$name $fmt"
      if (flag.isEmpty) partial else s"$partial|-$flag $fmt"
    }
  }

  def generalUsage = {
    val opt = optArgs.map(arg => s"[${generateOptArgFmt(arg)}]").mkString(" ")
    val pos = posArgs.map(s => s"${s.name}").mkString(" ")
    s"$opt $pos"
  }

  def getNonEmptyArgs[T <: Argument](args: Iterable[T]) = args.withFilter(!_.desc.isEmpty)

  def posArgsUsage = {
    val str = getNonEmptyArgs(posArgs).map { arg =>
      s"$defaultPrecedingIndent${arg.name}: ${arg.desc}"
    }.mkString("\n")
    if (!str.isEmpty) s"\nPositional arguments:\n$str\n" else ""
  }

  def optArgsUsage = {
    val str = getNonEmptyArgs(optArgs).map { arg =>
      val defaultVal = if (!arg.value.isEmpty) s" (default: ${arg.value}) " else " "
      val preceding = s"${generateOptArgFmt(arg)}:$defaultVal"
      val desc = arg.desc
      val wrap = if (preceding.length + desc.length > defaultMaxCol) {
        s"\n$defaultDescIndent"
      } else ""

      s"$defaultPrecedingIndent$preceding$wrap$desc"
    }.mkString("\n")
    if (!str.isEmpty) s"\nOptional arguments:\n$str\n" else ""
  }

  def description = if (!desc.isEmpty) s"\n$desc\n" else ""

  def wrapLine() = ???

  def getHelp = s"usage: $progName $generalUsage\n" +
      s"$description" +
      s"$posArgsUsage" +
      s"$optArgsUsage"
}