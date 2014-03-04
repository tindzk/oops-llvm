package org.oopsc.statement

import org.oopsc._
import org.oopsc.expression.{BooleanLiteralExpression, Expression}
import scala.collection.mutable.ListBuffer
import org.oopsc.expression.BooleanLiteralExpression
import org.llvm.binding.LLVMLibrary.LLVMIntPredicate
import org.llvm._
import org.oopsc.symbol.Block

class IfStatement(_condition: Expression, _thenStatements: Block) extends Statement {
  var branches = new ListBuffer[(Expression, Block)]
  branches += (_condition -> _thenStatements)

  var elseBranch = new Block

  override def refPass(sem: SemanticAnalysis) {
    for ((cond, stmts) <- this.branches) {
      cond.refPass(sem)
      cond.resolvedType.check(sem, sem.types.boolWrapper, cond.position)
      stmts.refPass(sem)
    }

    elseBranch.refPass(sem)
  }

  override def optimPass() : Statement = {
    this.branches = this.branches.map(b => (b._1.optimPass(), b._2.optimPass()))

    var newBranches = new ListBuffer[(Expression, Block)]

    /* Delete all branches that always evaluate to `false'. */
    var skipRest = false
    for ((cond, stmts) <- this.branches if !skipRest) {
      cond match {
        case BooleanLiteralExpression(false, _) =>
          /* Skip branch. */
        case BooleanLiteralExpression(true, _) =>
          newBranches += (cond -> stmts)
          /* Skip all other branches. */
          skipRest = true
        case _ =>
          newBranches += (cond -> stmts)
      }
    }

    this.branches = newBranches
    this.elseBranch = this.elseBranch.optimPass()

    /* If no branches left, return NullStatement. */
    if (this.branches.isEmpty && this.elseBranch.statements.isEmpty) {
      return new NullStatement
    }

    this
  }

  def addIfElse(condition: Expression, stmts: Block) {
    this.branches += (condition -> stmts)
  }

  def setElse(stmts: Block) {
    this.elseBranch = stmts
  }

  private def print(tree: TreeStream, condition: Expression, stmts: Block) {
    tree.println("BRANCH")
    tree.indent

    if (condition == null) {
      tree.println("DEFAULT")
    } else {
      condition.print(tree)
    }

    tree.indent
    stmts.print(tree)
    tree.unindent

    tree.unindent
  }

  override def print(tree: TreeStream) {
    tree.println("IF")
    tree.indent

    for ((cond, stmts) <- this.branches) {
      if (cond != null) {
        this.print(tree, cond, stmts)
      }
    }

    if (this.elseBranch.statements.nonEmpty) {
      this.print(tree, null, this.elseBranch)
    }

    tree.unindent
  }

  override def generateCode(code: CodeContext) {
    val end = code.currentFunction.appendBasicBlock("if.end")

    for (((cond, stmts), i) <- this.branches.zipWithIndex) {
      val ifTrue = code.currentFunction.appendBasicBlock(s"if.true.$i")
      val ifFalse = code.currentFunction.appendBasicBlock(s"if.false.$i")

      code.builder.buildCondBr(cond.generateCode(code, true), ifTrue, ifFalse)

      code.builder.positionBuilderAtEnd(ifTrue)
      stmts.generateCode(code, end)

      code.builder.positionBuilderAtEnd(ifFalse)
    }

    this.elseBranch.generateCode(code, end)

    code.builder.positionBuilderAtEnd(end)
  }
}