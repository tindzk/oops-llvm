package org.oopsc

import org.oopsc.scope._
import org.oopsc.symbol._
import scala.collection.mutable.ListBuffer

/**
 * Semantic analysis is the phase in which the compiler adds semantic
 * information to the parse tree and builds the symbol table. This phase
 * performs semantic checks such as type checking (checking for type errors),
 * or object binding (associating variable and function references with their
 * definitions), or definite assignment (requiring all local variables to be
 * initialized before use), rejecting incorrect programs or issuing warnings.
 * Semantic analysis usually requires a complete parse tree, meaning that this
 * phase logically follows the parsing phase, and logically precedes the code
 * generation phase, though it is often possible to fold multiple phases into
 * one pass over the code in a compiler implementation.
 *
 * From http://en.wikipedia.org/wiki/Semantic_analysis_%28compilers%29#Front_end
 */
class SemanticAnalysis {
  var currentScope: Option[Scope] = None
  var currentScopedType: ScopedType = null
  var currentMethod: MethodSymbol = null

  var types = new Types

  def enter(scope: Scope) = {
    scope match {
      case m: MethodSymbol => this.currentMethod = m
      case s: ScopedType => this.currentScopedType = s
      case _ =>
    }

    scope.enclosingScope = this.currentScope
    this.currentScope = Some(scope)
  }

  def leave() = {
    this.currentScope = this.currentScope match {
      case Some(s) => s.enclosingScope
      case None => throw new CompileException("Scope stack is empty.")
    }
  }

  def defineSymbol(sym: Type) = {
    sym.declaringContext = Some(this.currentScopedType)
    this.currentScope.get.defineSymbol(sym)
  }
}