package org.oopsc.statement

import org.oopsc.{CodeContext, SemanticAnalysis, TreeStream}

abstract class Statement {
  def returns() = false

  /**
   * Performs the definition pass of the semantic analysis.
   *
   * @param sem Context of the semantic analysis.
   */
  def refPass(sem: SemanticAnalysis) {

  }

  /**
   * Performs the optimisation pass.
   *
   * @return Optimised statement.
   */
  def optimPass() = this

  /**
   * Prints statement in a tree structure.
   *
   * @param tree Output stream.
   */
  def print(tree: TreeStream)

  /**
   * Generates assembly code for the statement. Requires prior completion of the
   * contextual analysis.
   *
   * @param code Output stream.
   */
  def generateCode(code: CodeContext)
}