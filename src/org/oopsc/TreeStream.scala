package org.oopsc

import java.io.OutputStream
import java.io.PrintStream

/**
 * Output stream structuring text in a tree. Provides methods for controlling
 * the indention depth.
 *
 * @param indentionStep Step size for each indention level.
 */
class TreeStream(stream: OutputStream, var indentionStep: Int) extends PrintStream(stream) {
  /**
   * Buffer for the last print character. If the last character was a newline (\n),
   * the input will be indented before the next character.
   */
  private var lastChar = 0

  /** Current indention depth. */
  private var indention = 0

  /**
   * Increases indention depth.
   */
  def indent {
    this.indention += this.indentionStep
  }

  /**
   * Decreases indention depth.
   */
  def unindent {
    this.indention -= this.indentionStep
    assert(this.indention >= 0)
  }

  /**
   * Overrides method write() of the super class ensuring that
   * the buffer is being indented properly.
   */
  override def write(buf: Array[Byte], off: Int, len: Int) {
    for (i <- off to len - 1) {
      this.write(buf(i))
    }
  }

  /**
   * Overrides method write() of the super class ensuring that
   * the value is being indented properly.
   */
  override def write(b: Int) {
    if (this.lastChar == '\n') {
      for (i <- 1 to this.indention) {
        super.write(' ')
      }
    }

    this.lastChar = b
    super.write(b)
  }
}