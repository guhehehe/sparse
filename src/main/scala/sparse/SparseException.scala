package sparse

class SparseException(msg: String, cause: Throwable) extends Exception(msg, cause)

class UnknownArgException(msg: String, cause: Throwable = null) extends SparseException(msg, cause)

class TooManyArgsException(msg: String, cause: Throwable = null) extends SparseException(msg, cause)

class TooFewArgsException(msg: String, cause: Throwable = null) extends SparseException(msg, cause)

class MissingValueException(msg: String, cause: Throwable = null) extends SparseException(msg, cause)

class ArgFormatException(msg: String, cause: Throwable = null) extends SparseException(msg, cause)
