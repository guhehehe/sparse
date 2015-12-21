package sparse

import org.scalatest.BeforeAndAfter

class TestSparse extends UnitSpec with BeforeAndAfter {
  val sparse = Sparse()

  /////////////////////////////////////////////////////////////////////////////
  // Test Sparse#addArg
  /////////////////////////////////////////////////////////////////////////////

  "`addArg`" should "throw IllegalArgumentException if the `name` argument is malformed" in {
    intercept[IllegalArgumentException] {
      sparse.addArg("-wrong-arg")
    }
    intercept[IllegalArgumentException] {
      sparse.addArg("wrong+arg")
    }
    intercept[IllegalArgumentException] {
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
    assertResult(false) {
      sparse.optArgs.contains("--name") | sparse.canonicalName.contains("-f")
    }
    val name = "optarg"
    val flag = "f"
    val value = "opt1"
    val desc = "desc"
    val opt = Set("opt1", "opt2")
    val newSparse = sparse.addArg(s"--$name", s"-$flag", value = value, desc = desc, options = opt)
    assertResult(true) {
      newSparse.optArgs.contains(s"--$name")
    }
    assertResult(Some(s"--$name")) {
      newSparse.canonicalName.get(s"-$flag")
    }
    val optArg = newSparse.optArgs(s"--$name")
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

  it should "throw IllegalArgumentException if `flag` does not start with a single '-'" in {
    intercept[IllegalArgumentException] {
      sparse.addArg("--arg", "f")
    }
    intercept[IllegalArgumentException] {
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

  "`parseHelper`" should "raise IllegalArgumentException when there's too few positional args" in {
    intercept[IllegalArgumentException] {
      sparse.addArg("posarg").parserHelper(Nil)
    }
    intercept[IllegalArgumentException] {
      sparse.addArg("--optarg").addArg("posarg1").addArg("posarg2").parserHelper("arg1" :: Nil)
    }
    intercept[IllegalArgumentException] {
      sparse.addArg("--optarg").addArg("posarg").parserHelper("--optarg":: "arg1" :: Nil)
    }
  }

  "`parseHelper`" should "raise IllegalArgumentException when there's too many positional args" in {
    intercept[IllegalArgumentException] {
      sparse.parserHelper("arg1" :: Nil)
    }
    intercept[IllegalArgumentException] {
      sparse.addArg("posarg1").parserHelper("arg1" :: "arg2" :: Nil)
    }
    intercept[IllegalArgumentException] {
      sparse.addArg("--optarg").parserHelper("arg1" :: Nil)
    }
    intercept[IllegalArgumentException] {
      sparse
        .addArg("--optarg")
        .addArg("posarg1")
        .addArg("posarg2")
        .parserHelper("--optarg":: "optarg" :: "posarg" :: Nil)
    }
  }
}
