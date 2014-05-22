package org.oopsc.symbol

import org.oopsc.scope._
import org.oopsc._
import org.llvm.{TypeRef, Value}

/* Variable declaration. Also used for method parameters. */
class VariableSymbol(identifier: Identifier) extends Type {
  var scope: Scope = null
  var typeIdent: Identifier = null
  var resolvedType: Option[Type] = None

  /* TODO There must be a table maintaining all variable declarations, linking it to the defining scope. */
  var llvmValue: Option[Value] = None

  def this(ident: Identifier, typeSymbol: Type) {
    this(ident)
    this.resolvedType = Some(typeSymbol)
    this.typeIdent = typeSymbol.ident()
  }

  def this(ident: Identifier, typeIdent: Identifier) {
    this(ident)
    this.typeIdent = typeIdent
  }

  override def ident() = identifier

  def getLLVMValue()(context: Value, code: CodeContext, deref: Boolean) = {
    val sym = this

    if (sym.llvmValue.isEmpty) {
      sym.llvmValue = Some(code.builder.buildAlloca(this.getLLVMType(), sym.name()))
    }

    if (deref) {
      code.builder.buildLoad(sym.llvmValue.get, sym.name())
    } else {
      sym.llvmValue.get
    }
  }

  /**
   * Returns the resolved type. Requires prior semantic analysis (definition pass
   * is sufficient).
   */
  def getResolvedType: Type = {
    if (this.resolvedType.isEmpty) {
      this.resolvedType = Some(this.scope.resolveClass(this.sem, typeIdent))
    }

    resolvedType.get
  }

  override def defPass(sem: SemanticAnalysis) {
    sem.defineSymbol(this)
    this.scope = sem.currentScope.get
  }

  private var sem: SemanticAnalysis = null
  override def refPass(sem: SemanticAnalysis) {
    this.sem = sem
    getResolvedType
  }

  override def getLLVMType() : TypeRef = {
    val t = this.resolvedType.get

    if (t.mustEmbed()) {
      t.getLLVMType()
    } else {
      t.getLLVMType().pointerType()
    }
  }

  override def print(tree: TreeStream) {
    tree.println(s"${this.name()}: " +
      this.resolvedType.map(_.name()).getOrElse("<unresolved>"))
  }
}