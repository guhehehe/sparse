package sparse

import org.scalatest.BeforeAndAfter

class TestSparse extends UnitSpec with BeforeAndAfter {
  val sparse = Sparse()

  /////////////////////////////////////////////////////////////////////////////
  // Test Sparse#addArg
  /////////////////////////////////////////////////////////////////////////////

  "`addArg`" should "throw ArgFormatException if the `name` argument is malformed" in {
    intercept[ArgFormatException] {
      sparse.addArg("-wrong-arg")
    }
    intercept[ArgFormatException] {
      sparse.addArg("wrong+arg")
    }
    intercept[ArgFormatException] {
      sparse.addArg("--wrong+arg")
    }
  }

  "Add positional argument" should "create positional argument and push into `posArgs`" in {
    assertResult(0) {
      sparse.posArgs.size
    }
    val name = "posarg"
    val desc = "desc"
    val opt = Set("opt1", "opt2")
    val newSparse = sparse.addArg(name, desc = desc, options = opt)
    assertResult(1) {
      newSparse.posArgs.size
    }
    val posArg = newSparse.posArgs(0)
    assertResult(name) {
      posArg.name
    }
    assertResult(desc) {
      posArg.desc
    }
    assertResult(opt) {
      posArg.options
    }
  }

  it should "ignore `flag` even if its malformed" in {
    assertResult("posarg") {
      sparse.addArg("posarg", "f").posArgs(0).name
    }
  }

  it should "ignore `value`" in {
    assertResult("") {
      sparse.addArg("posarg", value = "13").posArgs(0).value
    }
  }

  "Add optional argument" should "create proper mapping for optional arguments" in {
    val name = "optarg"
    val flag = "f"
    val value = "opt1"
    val desc = "desc"
    val opt = Set("opt1", "opt2")

    assertResult(false) {
      sparse.optArgs.contains(s"$name") | sparse.canonicalName.contains(s"$flag")
    }

    val newSparse = sparse.addArg(s"--$name", s"-$flag", value = value, desc = desc, options = opt)
    assertResult(true) {
      newSparse.optArgs.contains(s"$name")
    }
    assertResult(Some(s"$name")) {
      newSparse.canonicalName.get(s"$flag")
    }

    val optArg = newSparse.optArgs(s"$name")
    assertResult(name) {
      optArg.name
    }
    assertResult(value) {
      optArg.value
    }
    assertResult(flag) {
      optArg.flag
    }
    assertResult(desc) {
      optArg.desc
    }
    assertResult(opt) {
      optArg.options
    }
  }

  it should "throw ArgFormatException if `flag` does not start with a single '-'" in {
    intercept[ArgFormatException] {
      sparse.addArg("--arg", "f")
    }
    intercept[ArgFormatException] {
      sparse.addArg("--arg", "--f")
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Test Sparse#parse
  /////////////////////////////////////////////////////////////////////////////

  "`parse`" should "create Arguments obj and add all arguments to it" in {
    val args = sparse.addArg("arg1").addArg("arg2").addArg("--arg3")
    val value = "value"
    val parsedArgs = args.parse(Array("--arg3", value, value, value))

    args.posArgs.foreach { thisArg =>
      val name = parsedArgs.toCamelCase(thisArg.name)
      val thatArg = parsedArgs.args.get(name).get
      assertResult(true) {
        thisArg.name == thatArg.name
      }
      assertResult(true) {
        thatArg.value == value
      }
    }

    args.optArgs.foreach { thisTpl =>
      val (_, thisArg) = thisTpl
      val name = parsedArgs.toCamelCase(thisArg.name)
      val thatArg = parsedArgs.args.get(name).get
      assertResult(true) {
        thisArg.name == thatArg.name
      }
      assertResult(true) {
        if (name == "help") {
          thatArg.value == "false"
        } else {
          thatArg.value == value
        }
      }
    }

    // if the above tests are passed, this ensures that the args added to Sparse are exactly the
    // same args in the parsed Arguments obj
    assertResult(parsedArgs.args.size) {
      args.posArgs.size + args.optArgs.size
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Test Sparse#parseHelper
  /////////////////////////////////////////////////////////////////////////////

  "`parseHelper`" should "set proper value for added positional arguments" in {
    def testAdded(added: Sparse, num: Int) = {
      assertResult(num) {
        added.posArgs.size
      }
      added.posArgs.foreach { arg =>
        assertResult(true)(arg.value.isEmpty)
      }
    }

    def testParsed(added: Sparse, parsed: Sparse, values: Array[String]) = {
      val length = added.posArgs.size
      for (i <- 0 until length) {
        assertResult(true) {
          parsed.posArgs(i).name == added.posArgs(i).name
        }
        assertResult(values(i)) {
          parsed.posArgs(i).value
        }
      }
    }

    assertResult(0) {
      sparse.posArgs.size
    }

    val added = sparse.addArg("posarg1").addArg("posarg2")
    testAdded(added, 2)

    val parsed = added.parserHelper("arg1" :: "arg2" :: Nil)
    testParsed(added, parsed, Array("arg1", "arg2"))

    val added2 = sparse.addArg("posarg1").addArg("posarg2").addArg("--optarg")
    testAdded(added2, 2)

    val parsed2 = added2.parserHelper("--optarg" :: "opt" :: "arg1" :: "arg2" :: Nil)
    testParsed(added2, parsed2, Array("arg1", "arg2"))
  }

  it should "set proper value for added optional arguments" in {
    val nameValue = Map("arg1" -> "val1", "arg2" -> "val2")
    nameValue.foreach {
      case (name, _) => assertResult(false) {
        sparse.optArgs.contains(name)
      }
    }

    val added = nameValue.foldLeft(sparse) { (init, acc) => init.addArg(s"--${acc._1}") }
    nameValue.foreach {
      case (name, value) => {
        assertResult(true) {
          added.optArgs.contains(name)
        }
        assertResult(true) {
          added.optArgs(name).value.isEmpty
        }
      }
    }

    val args = nameValue.foldRight(Nil: List[String]) { (acc, init) => s"--${acc._1}" :: acc._2 :: init }
    val parsed = added.parserHelper(args)
    nameValue.foreach {
      case (name, value) => assertResult(value) {
        parsed.optArgs(name).value
      }
    }
  }

  it should "raise TooFewArgsException when there's too few positional args" in {
    intercept[TooFewArgsException] {
      sparse.addArg("posarg").parserHelper(Nil)
    }
    intercept[TooFewArgsException] {
      sparse.addArg("--optarg").addArg("posarg1").addArg("posarg2").parserHelper("arg1" :: Nil)
    }
    intercept[TooFewArgsException] {
      sparse.addArg("--optarg").addArg("posarg").parserHelper("--optarg":: "arg1" :: Nil)
    }
  }

  it should "raise TooManyArgsException when there's too many positional args" in {
    intercept[TooManyArgsException] {
      sparse.parserHelper("arg1" :: Nil)
    }
    intercept[TooManyArgsException] {
      sparse.addArg("posarg1").parserHelper("arg1" :: "arg2" :: Nil)
    }
    intercept[TooManyArgsException] {
      sparse.addArg("--optarg").parserHelper("arg1" :: Nil)
    }
    intercept[TooManyArgsException] {
      sparse
        .addArg("--optarg")
        .addArg("posarg1")
        .parserHelper("--optarg":: "optarg" :: "posarg" :: "posarg" :: Nil)
    }
  }

  it should "raise UnknownArgException when opt arg appears after pos arg" in {
    intercept[UnknownArgException] {
      sparse.addArg("--optarg").addArg("posarg1").parserHelper("arg1" :: "--optarg" :: "value" :: Nil)
    }
  }

  it should "raise MissingValueException when missing value for opt arg" in {
    intercept[MissingValueException] {
      sparse.addArg("--optarg").parserHelper("--optarg" :: Nil)
    }
  }

  it should "raise UnknownArgException when wrong opt is provided" in {
    intercept[UnknownArgException] {
      sparse.addArg("--optarg").parserHelper("--other" :: "value" :: Nil)
    }
  }

  it should "negate the default value when switch arg is present"
}