package org.oopsc.symbol

import scala.collection.mutable.ListBuffer
import org.oopsc.statement.{NullStatement, ThrowStatement, ReturnStatement, Statement}
import org.oopsc.{CompileException, CodeContext, TreeStream, SemanticAnalysis}
import org.oopsc.Position
import org.llvm.BasicBlock

class Block(stmts : ListBuffer[Statement] = new ListBuffer[Statement]) {
  var statements = stmts

  def refPass(sem: SemanticAnalysis) {
    for (stmt <- this.statements) {
      stmt.refPass(sem)

      if (!(stmt eq this.statements.last) && stmt.returns()) {
        /* TODO Statements must store position. */
        throw new CompileException("A returning statement is only allowed at the end of a block.", new Position())
      }
    }
  }

  def optimPass() = {
    this.statements.map(_.optimPass())

    /* Filter out NullStatements which may be a result of the above optimisation pass. */
    this.statements.filterNot(_.isInstanceOf[NullStatement])

    this
  }

  def generateCode(code: CodeContext, then: BasicBlock) {
    this.statements.foreach(_.generateCode(code))

    if (this.statements.find(_.returns()).isEmpty) {
      code.builder.buildBr(then)
    }
  }

  def print(tree: TreeStream) {
    if (!this.statements.isEmpty) {
      tree.indent
      this.statements.foreach(_.print(tree))
      tree.unindent
    }
  }
}
