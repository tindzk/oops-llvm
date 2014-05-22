package org.oopsc

object CompileException {
  def formatError(position: Position, errorLine: String, errorStart: Int, errorEnd: Int) {
    var out = errorLine + "\n"

    for (i <- 0 to position.column) {
      out += " "
    }

    out += "\n"

    if (errorStart >= 0 && errorEnd >= 0) {
      for (i <- errorStart to errorEnd) {
        out += "^"
      }
    }
  }
}

/**
 * Default exception to be thrown upon compilation errors.
 */
class CompileException(message: String) extends Exception(message) {
  def this(message: String, position: Position) = this(s"$position: $message")

  def this(message: String, position: Position, errorLine: String, errorStart: Int, errorEnd: Int) = {
    this(s"$position: $message\n" + CompileException.formatError(position, errorLine, errorStart, errorEnd))
  }
}