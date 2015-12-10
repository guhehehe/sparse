package sparse

import scala.annotation.tailrec
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}


object ArgumentParser {
  def apply() = new ArgumentParser()
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
 *
 */
class ArgumentParser private(
    private val positionalArguments: Vector[PositionalArgument],
    private val optionalArguments: Map[String, OptionalArgument]) {

  def this() = this(Vector.empty[PositionalArgument], Map.empty[String, OptionalArgument])

  // TODO: choose from (options)
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
   * @return a new [[ArgumentParser]] with this new argument added
   */
  def addArg(name: String, flag: String = "", value: String = ""): ArgumentParser = {
    val parsedVal = Value.unapply(value)
    name match {
      case PositionalArgument(name) => {
        val position = positionalArguments.length
        new ArgumentParser(
          positionalArguments :+ PositionalArgument(position, name),
          optionalArguments
        )
      }
      case OptionalArgument(name, isFlag) if !isFlag => {
        val argObj = OptionalArgument(name, parsedVal)
        val updates: Map[String, OptionalArgument] = flag match {
          case "" => Map(name -> argObj)
          case OptionalArgument(flag, isFlag) if isFlag => {
            val newArgObj = argObj.setFlag(flag)
            Map(name -> newArgObj, flag -> newArgObj)
          }
          case unknown => throw new IllegalArgumentException(
            s"Can't handle flag: $unknown, make sure flag argument is prefixed by a single '-'."
          )
        }
        new ArgumentParser(positionalArguments, optionalArguments ++ updates)
      }
      case unknown => throw new IllegalArgumentException(s"Can't handle argument: $unknown.")
    }
  }

  private[this] def errorExit(message: String): ArgumentParser = {
    System.err.println(message)
    System.exit(1)
    this
  }

  /** Set value for positional arguments */
  private[this] def setVal(position: Int, value: String): ArgumentParser = {
    val arg = positionalArguments(position)
    new ArgumentParser(
      positionalArguments.updated(position, arg.setValue(value)),
      optionalArguments
    )
  }

  /** Set value for optional arguments */
  private[this] def setVal(arg: OptionalArgument, value: String): ArgumentParser = {
    val newKey = arg.setValue(value)
    val updates: Map[String, OptionalArgument] = arg.flag match {
      case Some(flag) => Map(arg.name -> newKey, flag -> newKey)
      case None => Map(arg.name -> newKey)
    }
    new ArgumentParser(positionalArguments, optionalArguments ++ updates)
  }


  @tailrec
  private final def parserHelper(
      args: List[String],
      lastPosition: Int = -1,
      cutoff: Boolean = false): ArgumentParser = {

    if (args.length > 0 & lastPosition >= positionalArguments.length) {
      errorExit(s"Too many positional arguments.")
    }
    args match {
      // deal with positional arguments
      case Value(value) :: etc => {
        val newPosition = lastPosition + 1
        setVal(newPosition, value).parserHelper(etc, newPosition, true)
      }
      // deal with optional arguments
      case OptionalArgument(arg, _) :: Value(value) :: etc if !cutoff => {
        Try(optionalArguments(arg)) match {
          case Success(argObj) => argObj.isSwitch match {
            // switch argument's value is preset, so skip it
            case true => parserHelper(args.drop(1), lastPosition)
            // deal with other optional arguments, which fall into the following cases:
            //   prog -f value
            //   prog --arg value
            case false => setVal(argObj, value).parserHelper(etc, lastPosition)
          }
          case Failure(e: NoSuchElementException) => {
            errorExit(s"Unknown optional argument: ${args.head}.")
          }
          case Failure(NonFatal(e)) => throw e
        }
      }
      case Nil => {
        if (lastPosition != positionalArguments.length - 1) {
          errorExit(s"Too few positional arguments.")
        }
        this
      }
      case _ => errorExit(s"Illegal argument: ${args.head}.")
    }
  }

  private[this] def foldFunc(args: Arguments, arg: Argument): Arguments = arg.value match {
    case Some(value) => args.set(arg.name, value)
    case _ => args
  }

  def parse(arguments: Array[String]): Arguments = {
    val args = new Arguments()
    val p = parserHelper(arguments.toList)
    p.optionalArguments.values.foldLeft(p.positionalArguments.foldLeft(args)(foldFunc))(foldFunc)
  }
}
