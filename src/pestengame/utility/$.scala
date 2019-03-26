package pestengame.utility

/**
  * Defines a set of helper functions.
  */
object $ {
  val prefix = "[PESTEN] "

  def printlnP(message: String): Unit = {
    Console.println(concat(prefix, message))
  }

  def printfP(message: String, args: Any*): Unit = {
    Console.printf(concat(prefix, message), args: _*)
  }

  def concat(a: String, b: String): String = {
    a + b
  }

  def callIf(condition: Boolean, callback: () => Unit): Unit = {
    if (condition) callback()
  }

  def isNull(value: Any): Boolean = {
    value == null
  }

  def isNotNull(value: Any): Boolean = {
    value != null
  }

  def oneOf[T](a: T, b: T): T = {
    if (a == null) b else a
  }
}
