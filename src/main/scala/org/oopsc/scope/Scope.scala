package org.oopsc.scope

import org.oopsc.{SemanticAnalysis, Identifier, CompileException}
import scala.collection.mutable.LinkedHashMap
import org.oopsc.symbol.{ScopedType, Type, ClassType}

trait Scope {
  /** Scope in which this scope defined. For the global scope, the value is None. */
  var enclosingScope: Option[Scope] = None

  /** Symbol table that maps an identifier to a type object. */
  protected var symbols = new LinkedHashMap[String, Type]()

  /**
   * The parent scope denotes where to look next upon a type lookup, i.e.,
   * in the superclass or in the enclosing scope (default). This method may be overwritten
   * by a type class.
   */
  def getParentScope: Option[Scope] = this.enclosingScope

  /** Returns the scope name. */
  def getScopeName: String

  /** Define a type in the current scope. */
  def defineSymbol(sym: Type) {
    if (this.symbols.contains(sym.name())) {
      val pos = this.symbols.get(sym.name()).get.ident().position
      throw new CompileException(s"Redeclaration of type ${sym.name()} (declared in ${pos.line}:${pos.column}).",
        sym.pos())
    }

    this.symbols += ((sym.name(), sym))
  }

  /** Look up the passed identifier in this scope, or in parent scope if not declared here. */
  protected def resolve(sem: SemanticAnalysis, ident: Identifier, requestingClass: Option[ScopedType]): Option[Type] = {
    this.symbols.get(ident.name) match {
      case Some(sym) =>
        if (!sym.availableFor(sem, requestingClass)) {
          if (requestingClass.isDefined) {
            throw new CompileException(s"Symbol ${ident.name} not accessible from within ${requestingClass.get.name()}.${this.getScopeName}.", ident.position)
          } else {
            throw new CompileException(s"Symbol ${ident.name} not accessible from within ${this.getScopeName}.", ident.position)
          }
        }

        return Some(sym)
      case None => None
    }

    this.getParentScope match {
      case Some(s) => s.resolve(sem, ident, requestingClass)
      case None => None
    }
  }

  def resolveSymbol(sem: SemanticAnalysis, ident: Identifier, requestingClass: Option[ScopedType]): Type =
    resolve(sem, ident, requestingClass) match {
      case Some(v) => v
      case None => throw new CompileException(s"Symbol ${ident.name} not found in scope '${this.getScopeName}'.", ident.position)
    }

  def resolveClass(sem: SemanticAnalysis, ident: Identifier): Type = {
    /* Resolve in current and in system namespace. */
    val curNs = resolve(sem, ident, None)

    /* TODO This is a hack. */
    val sysNs = resolve(sem, new Identifier("monty.stdlib." + ident.name), None)

    (curNs, sysNs) match {
      case (Some(c: Type), _) => c
      case (_, Some(c: Type)) => c
      case (Some(c), _) => throw new CompileException(s"${ident.name} is not a class.", ident.position)
      case _ => throw new CompileException(s"Class ${ident.name} not found.", ident.position)
    }
  }
}