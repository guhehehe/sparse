package sparse

import scala.language.dynamics

class Arguments private(private val args: Map[String, String]) extends Dynamic {
  def this() = this(Map.empty[String, String])

  def selectDynamic(name: String): String = args(name)

  def set(name: String, value: String): Arguments = {
    new Arguments(args + (toCamelCase(name) -> value))
  }

  override def toString = args.toString()

  private def toCamelCase(name: String) = name.replace('-', '_')
      .split('_')
      .filter(_.length() > 0)
      .foldLeft("") { (output, input) =>
    if (output == "") {
      input.toLowerCase()
    } else {
      s"$output${input.take(1).toUpperCase()}${input.drop(1).toLowerCase()}"
    }
  }
}
