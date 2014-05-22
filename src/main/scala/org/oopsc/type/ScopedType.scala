package org.oopsc.symbol

import org.oopsc.scope._
import org.oopsc.CodeContext
import org.llvm.Value

abstract class ScopedType extends Type with Scope {
  override def getScopeName = this.name()

  /* TODO Move instantiate(). */
  def instantiate(code: CodeContext, arguments: Value*) : Value = null
}
