package org.oopsc.statement

import org.oopsc._
import org.oopsc.symbol._
import org.oopsc.expression._

class ReturnStatement(var position: Position, var value: Expression = null) extends Statement {
  protected var method: MethodSymbol = null

  override def refPass(sem: SemanticAnalysis) {
    this.method = sem.currentMethod
    val retType = sem.currentMethod.getResolvedReturnType(sem)

    if (this.value == null) {
      if (retType ne Types.voidType) {
        throw new CompileException(s"Return value of type ${retType.name} expected.", this.position)
      }
    } else {
      this.value.refPass(sem)

      if (retType eq Types.voidType) {
        throw new CompileException("No return value expected.", this.value.position)
      }

      this.value.resolvedType.check(sem, retType, this.value.position)
    }
  }

  override def optimPass() : Statement = {
    if (this.value != null) {
      this.value = this.value.optimPass()
    }

    this
  }

  override def print(tree: TreeStream) {
    tree.println("RETURN")

    if (this.value != null) {
      tree.indent
      this.value.print(tree)
      tree.unindent
    }
  }

  override def generateCode(code: CodeContext) {
    if (this.value != null) {
      code.builder.buildStore(this.value.generateCode(code, true), code.retValue)
    }

    code.builder.buildBr(code.retBlock)
  }

  override def returns() = true
}