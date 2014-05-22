package org.oopsc.statement

import org.oopsc._
import org.oopsc.expression._
import org.oopsc.symbol._
import scala.collection.mutable.ListBuffer
import org.llvm.{BasicBlock, TypeRef, Value}
import org.llvm.binding.LLVMLibrary
import org.llvm.binding.LLVMLibrary.LLVMCallConv

/**
 * Implements a TRY statement which is used for exception handling.
 */
class TryStatement(var tryStatements: Block, position: Position) extends Statement {
  /**
   * CATCH branches assigning a statement block to a value that needs to be caught in order for
   * the statements to be executed.
   */
  var catchStatements = new ListBuffer[(ListBuffer[LiteralExpression], Block)]

  def addCatchBlock(condition: ListBuffer[LiteralExpression], stmts: Block) {
    this.catchStatements += (condition -> stmts)
  }

  override def print(tree: TreeStream) {

  }

  override def generateCode(code: CodeContext) {

  }
}