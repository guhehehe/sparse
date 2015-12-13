package sparse

import scala.language.dynamics

sealed class Arguments private(
    private val args: Map[String, Argument]) extends Dynamic {

  private[sparse] def this() = this(Map.empty[String, Argument])

  def selectDynamic(name: String): String = args(name).value

  def set(name: String, value: Argument): Arguments = {
    new Arguments(args + (toCamelCase(name) -> value))
  }

  override def toString = {
    val kv = args.map {
      case (k, v) => {
        if (!v.value.isEmpty) {
          s"$k=${v.value}"
        } else {
          s"$k=NULL"
        }
      }
    }
    s"Arguments(${kv.mkString(", ")})"
  }

  private[sparse] def toCamelCase(name: String) = name.replace('-', '_').toLowerCase()
      .split('_')
      .filter(!_.isEmpty)
      .foldLeft("") {
    (output, input) =>
      if (output == "") {
        // skip the first word
        input
      } else {
        // capitalize the first letter of the reset of the words
        s"$output${input.take(1).toUpperCase()}${input.drop(1)}"
      }
  }
}
