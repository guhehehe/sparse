package sparse

import scala.annotation.tailrec
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

object Sparser {
  def apply() = new Sparser()
}

/**
 * An immutable argument parser.
 *
 * Arguments can be positional or optional. Positional arguments are mandatory, they need to be
 * passed explicitly to your program, while optional argument don't. If there were any positional
 * arguments, they should appear after the optional ones. This class provides methods for parsing
 * such CLI arguments.
 *
 * ==Overview==
 * {{{
 * import sparse.Preamble._
 *
 * val args = Array("--optional", "2", "-f", "http://api.api.com", "2.3")
 *
 * val arguments = ArgumentParser()
 *     .addArg("--flag", "-f", "false")
 *     .addArg("--optional")
 *     .addArg("uri")
 *     .addArg("double")
 *     .parse(args)
 *
 * val uri: java.net.URI = arguments.uri
 *
 * val double: Double = arguments.double
 *
 * println(uri.getScheme())
 *
 * println(double + 1)
 * }}}
 *
 * To use this class, you first need to config the arguments you expect to be parsed by calling
 * [[addArg]]. After configuring all the arguments, you can pass your arguments to [[parse]], and
 * you will get an [[Arguments]] object where you can get the value of the arguments by referencing
 * their names.
 *
 * The first argument of this method determines the name of the argument. If you prefix the name
 * with "--", it will be interpreted as an optional argument, otherwise it will be interpreted as
 * positional. The name may contain "-", in which case the name will be converted to camel case
 * when referencing from [[Arguments]] object:
 *
 * {{{
 * scala> val args = ArgumentParser().addArg("--camel-case").parse(Array("--camel-case", "3"))
 * args: sparse.Arguments = sparse.Arguments@75e5835c
 *
 * scala> args.camelCase
 * sparse.Value = 3
 * }}}
 */
class Sparser private(
    val title: String,
    val desc: String,
    private val posArgs: Vector[PositionalArg],
    private val optArgs: Map[String, OptionalArg],
    private val canonicalName: Map[String, String]) {

  def this(title: String = "", desc: String = "") = {
    this(title, desc, Vector.empty, Map.empty, Map.empty)
  }

  private[sparse] def update(
      title: String = this.title,
      desc: String = this.desc,
      posArgs: Vector[PositionalArg] = this.posArgs,
      optArgs: Map[String, OptionalArg] = this.optArgs,
      canonicalName: Map[String, String] = this.canonicalName): Sparser = {
    new Sparser(title, desc, posArgs, optArgs, canonicalName)
  }

  /**
   * Add a new argument to the parser.
   *
   * The $name of the argument, if prefixed by "--", will be interpreted as optional, otherwise it
   * will be interpreted as positional (mandatory).
   *
   * $flag only applies to optional argument, it will be ignored if the argument is positional.
   * Also, it '''must ''' be prefixed with "-".
   *
   * @param name the name of this argument
   * @param flag the shorthand representation of this argument
   * @param value the default value for this argument
   * @param options the options that the $value can be chosen from
   * @return a new [[Sparser]] with this new argument added
   */
  def addArg(
      name: String,
      flag: String = "",
      value: String = "",
      options: Set[String] = Set.empty,
      desc: String = ""): Sparser = {

    val parsedVal = Value.unapply(value).getOrElse("")
    name match {
      case PositionalArg(name) => {
        val position = posArgs.length
        val argObj = PositionalArg(position, name, options = options, desc = desc)
        update(posArgs = posArgs :+ argObj)
      }
      case arg@OptionalArg(name, isFlag) if !isFlag => {
        val argObj = OptionalArg(name, parsedVal, options = options).setFlag(flag)
        val newCname = if (!argObj.flag.isEmpty) {
          canonicalName + (flag -> arg)
        } else {
          canonicalName
        }
        update(optArgs = optArgs + (arg -> argObj), canonicalName = newCname)
      }
      case unknown => throw new IllegalArgumentException(s"Can't handle argument: $unknown.")
    }
  }

  def parse(arguments: Array[String]): Arguments = {
    val args = new Arguments()
    val p = parserHelper(arguments.toList)
    p.optArgs.values.foldLeft(p.posArgs.foldLeft(args)(foldFunc))(foldFunc)
  }

  private[this] def foldFunc(args: Arguments, arg: Argument): Arguments = {
    args.set(arg.name, arg)
  }

  /** Set value for positional arguments */
  private[this] def setVal(position: Int, value: String): Sparser = {
    if (position >= posArgs.length) {
      errorExit(s"Too many positional arguments.")
    }
    val arg = posArgs(position)
    update(posArgs = posArgs.updated(position, arg.setValue(value)))
  }

  /** Set value for optional arguments */
  private[this] def setVal(arg: OptionalArg, value: String): Sparser = {
    val newArg = arg.setValue(value)
    update(optArgs = optArgs + (arg.name -> newArg))
  }

  @tailrec
  private[sparse] final def parserHelper(
      args: List[String],
      lastPosition: Int = -1,
      cutoff: Boolean = false): Sparser = args match {
    // deal with positional arguments
    case Value(value) :: etc => {
      val newPosition = lastPosition + 1
      setVal(newPosition, value).parserHelper(etc, newPosition, true)
    }
    // deal with optional arguments
    case OptionalArg(arg, _) :: Value(value) :: etc if !cutoff => {
      val cname = canonicalName.get(args.head) match {
        case Some(name) => name
        case _ => args.head
      }
      Try(optArgs(cname)) match {
        case Success(argObj) => argObj.isSwitch match {
          // negate the default value
          case true => {
            val switchOn = (!argObj.value.toBoolean).toString
            setVal(argObj, switchOn).parserHelper(args.drop(1), lastPosition)
          }
          case false => setVal(argObj, value).parserHelper(etc, lastPosition)
        }
        case Failure(e: NoSuchElementException) => {
          errorExit(s"Unknown optional argument: ${args.head}.")
        }
        case Failure(NonFatal(e)) => throw e
      }
    }
    case Nil => {
      if (lastPosition != posArgs.length - 1) {
        errorExit(s"Too few positional arguments.")
      }
      this
    }
    case _ => errorExit(s"Illegal argument: ${args.head}.")
  }

  private[this] def errorExit(message: String): Sparser = {
    System.err.println(message)
    System.exit(1)
    this
  }
}

object Main extends App {

  override val args: Array[String] = Array(
    "--optional-arg",
    "o2",
    "-f",
    "http://api.api.com",
    "2.3"
  )

  val arguments = Sparser()
      .addArg("--flag", "-f", "false", desc = "abc")
      .addArg("--optional-arg", options = Set("o1", "o2", "o3"))
      .addArg("uri")
      .addArg("double")
      .parse(args)

  println(arguments)

}
