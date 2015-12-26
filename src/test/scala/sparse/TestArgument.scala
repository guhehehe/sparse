package sparse

class TestArgument extends UnitSpec {

  class TestArgument(
      name: String,
      value: String,
      opt: Set[String]) extends Argument(name, value, opt) {
    override def setValue(newValue: String): Argument = ???
  }

  /////////////////////////////////////////////////////////////////////////////
  // Test Value#unapply
  /////////////////////////////////////////////////////////////////////////////

  "Value extractor" should "match any non empty string not starting with -" in {
    Value.unapply("-value") should be(None)
    Value.unapply("") should be(None)
    Value.unapply("value") should be(Some("value"))
  }

  /////////////////////////////////////////////////////////////////////////////
  // Test Argument constructor
  /////////////////////////////////////////////////////////////////////////////

  "Creating new instance of Argument" should
      "throw UnknownArgException if the value is not in options" in {
    intercept[UnknownArgException] {
      new TestArgument("name", value = "value", opt = Set("value1"))
    }
  }
}
