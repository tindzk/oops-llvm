package org.oopsc.statement

import org.oopsc.{CodeContext, TreeStream}

class NullStatement extends Statement {
  def print(tree: TreeStream) {
    tree.println("NOP")
  }

  override def generateCode(code: CodeContext) {

  }
}