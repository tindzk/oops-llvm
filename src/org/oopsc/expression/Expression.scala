package org.oopsc.expression

import org.oopsc._
import org.oopsc.symbol._
import java.io.{ UnsupportedEncodingException, ByteArrayOutputStream }
import org.oopsc.statement.ThrowStatement
import org.llvm.Value

/**
 * Base class for all expressions. Provides methods for boxing/unboxing and
 * dereferencing.
 */
abstract class Expression(var position: Position) {
  def resolvedType() : Type = {
    throw new CompileException("Type was not resolved.", position)
  }

  /**
   * True if the expression is a reference to a variable.
   */
  def isVariable() = false

  /**
   * Performs the reference pass of the semantic analysis.
   *
   * @param sem Context of the semantic analysis.
   */
  def refPass(sem: SemanticAnalysis) {

  }

  /**
   * Performs the optimisation pass.
   *
   * @return Optimised expression.
   */
  def optimPass() = this

  /**
   * Prints the expression in a tree structure.
   *
   * @param tree Output stream.
   */
  def print(tree: TreeStream)

  /**
   * Generates assembly code for the expression. Requires prior completion of the
   * contextual analysis.
   *
   * @param code Output stream.
   */
  def generateCode(code: CodeContext, deref: Boolean) : Value

  override def toString: String = {
    val stream = new ByteArrayOutputStream
    val tree: TreeStream = new TreeStream(stream, 4)

    this.print(tree)

    try {
      return stream.toString("UTF-8")
    } catch {
      case e: UnsupportedEncodingException => {
        return null
      }
    }
  }
}