package sparse

import scala.annotation.tailrec
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

object Sparse {
  val helpName = "help"
  val helpFlag = "h"
  val defaultProgName = "prog"

  def apply(progName: String = defaultProgName, desc: String = "") = {
    val help = new OptionalArg(helpName, "false", helpFlag, desc = "print this help message")
    new Sparse(progName, desc, Vector.empty, Map(helpName -> help), Map(helpFlag -> helpName))
  }
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
class Sparse private(
    val progName: String,
    val desc: String,
    private[sparse] val posArgs: Vector[PositionalArg],
    private[sparse] val optArgs: Map[String, OptionalArg],
    private[sparse] val canonicalName: Map[String, String]) {

  import Sparse._

  private[sparse] def update(
      progName: String = this.progName,
      desc: String = this.desc,
      posArgs: Vector[PositionalArg] = this.posArgs,
      optArgs: Map[String, OptionalArg] = this.optArgs,
      canonicalName: Map[String, String] = this.canonicalName): Sparse = {
    new Sparse(progName, desc, posArgs, optArgs, canonicalName)
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
   * @param prefixedFlag the shorthand representation of this argument with prefix
   * @param value the default value for this argument
   * @param options the options that the $value can be chosen from
   * @return a new [[Sparse]] with this new argument added
   */
  def addArg(
      name: String,
      prefixedFlag: String = "",
      value: String = "",
      options: Set[String] = Set.empty,
      desc: String = ""): Sparse = {

    val parsedVal = Value.unapply(value).getOrElse("")
    name match {
      case PositionalArg(name) => {
        val position = posArgs.length
        val argObj = new PositionalArg(position, name, options = options, desc = desc)
        update(posArgs = posArgs :+ argObj)
      }
      case prefixedName@OptionalArg(name) => {
        validateName(prefixedName)
        validateFlag(prefixedFlag)
        val flag = prefixedFlag.stripPrefix("-")
        val argObj = new OptionalArg(name, parsedVal, options = options, desc = desc)
            .setFlag(flag)
        val newCname = if (argObj.flag.nonEmpty) {
          canonicalName + (flag -> name)
        } else canonicalName
        update(optArgs = optArgs + (name -> argObj), canonicalName = newCname)
      }
      case unknown => throw new ArgFormatException(s"Can't handle argument: $unknown.")
    }
  }

  private def validateName(name: String) = {
    if (!name.startsWith("--")) {
      throw new ArgFormatException("Optional argument's name should start with --.")
    }
  }

  private def validateFlag(flag: String) = {
    if (flag.nonEmpty && !flag.startsWith("-")) {
      throw new ArgFormatException("Short optional argument's name should be start with -.")
    }
  }

  def parse(arguments: Array[String]): Arguments = {
    val args = new Arguments()
    val p = Try(parserHelper(arguments.toList)) match {
      case Success(parsed) => parsed
      case Failure(e: SparseException) => errorExit(e)
      case Failure(NonFatal(e)) => throw e
    }
    p.optArgs.values.foldLeft(p.posArgs.foldLeft(args)(foldFunc))(foldFunc)
  }

  private[this] def foldFunc(args: Arguments, arg: Argument): Arguments = args.set(arg.name, arg)

  /** Set value for positional arguments */
  private[sparse] def setVal(position: Int, value: String): Sparse = {
    if (position >= posArgs.length) {
      throw new TooManyArgsException("Too many positional arguments.")
    }
    val arg = posArgs(position)
    update(posArgs = posArgs.updated(position, arg.setValue(value)))
  }

  /** Set value for optional arguments */
  private[sparse] def setVal(arg: OptionalArg, value: String): Sparse = {
    val newArg = arg.setValue(value)
    update(optArgs = optArgs + (arg.name -> newArg))
  }

  @tailrec
  private[sparse] final def parserHelper(
      args: List[String],
      lastPosition: Int = -1,
      cutoff: Boolean = false): Sparse = args match {
    // deal with positional arguments
    case Value(value) :: etc => {
      val newPosition = lastPosition + 1
      setVal(newPosition, value).parserHelper(etc, newPosition, true)
    }
    // deal with optional arguments
    case OptionalArg(arg) :: etc if !cutoff => {
      val cname = canonicalName.get(arg) match {
        case Some(name) => name
        case _ => arg
      }
      if (cname == helpName) {
        printHelp()
      }
      optArgs.get(cname) match {
        case Some(argObj) => argObj.isSwitch match {
          // negate the default value
          case true => {
            val switchOn = (!argObj.value.toBoolean).toString
            setVal(argObj, switchOn).parserHelper(etc, lastPosition)
          }
          case false if etc.nonEmpty && Value.unapply(etc.head).nonEmpty => {
            setVal(argObj, etc.head).parserHelper(etc.drop(1), lastPosition)
          }
          case _ => {
            throw new MissingValueException(s"Missing value for optional argument ${args.head}.")
          }
        }
        case _ => throw new UnknownArgException(s"Unknown optional argument: ${args.head}.")
      }
    }
    case Nil => {
      if (lastPosition != posArgs.length - 1) {
        throw new TooFewArgsException("Too few positional arguments.")
      } else this
    }
    case _ => throw new UnknownArgException(s"Can't handle argument: ${args.head}.")
  }

  private[this] def printHelp() = {
    val formatter = new Formatter(progName, desc, posArgs, optArgs.values)
    println(formatter.getHelp)
    System.exit(0)
  }

  private[this] def errorExit(e: Throwable): this.type = {
    System.err.println(e.getMessage())
    System.exit(1)
    this
  }
}