package org.oopsc

import org.oopsc.statement._
import org.oopsc.symbol.{Block, MethodSymbol}
import scala.collection.mutable.ListBuffer
import org.oopsc.expression.BooleanLiteralExpression

/**
 * @note The term `termination' may be misleading. Here, it is understood in its
 * static sense when analysing the code during compilation time. Therefore,
 * considering a method, we are merely determining statically if all possible
 * branches are either returning a value or throwing an exception (i.e., jumping
 * out of the method). This means, that at least one return or throw statement
 * is reachable.
 *
 * However, a method identified as `terminating' by our algorithm may still not
 * terminate during run-time (if a branch is taken that leads to an endless
 * loop).
 *
 * Knowing whether a method terminates allows further code optimisations. See
 * also MethodSymbol.
 */
class Branch {
  var terminates = false
  var sub = new ListBuffer[Branch]
}

object BranchEvaluator {
  /**
   * Constructs a tree for the given statements, notably all branches. This
   * method is called recursively, taking into account all nesting levels.
   */
  protected def constructTree(sem: SemanticAnalysis, parent: Branch, stmts: Block) {
    for (stmt <- stmts.statements) {
      stmt match {
        case ifStmt: IfStatement =>
          for ((cond, stmts) <- ifStmt.branches) {
            val branchIf = new Branch
            parent.sub += branchIf

            constructTree(sem, branchIf, stmts)

            if (branchIf.terminates) {
              cond match {
                case BooleanLiteralExpression(true, _) =>
                  parent.terminates = true
                case _ =>
              }
            }
          }

          /* Requires that elseStatements always contains an entry for the else-block
           * even if it is empty. */
          val branch = new Branch
          parent.sub += branch
          constructTree(sem, branch, ifStmt.elseBranch)

          if (branch.terminates) {
            parent.terminates = true
          }

        case whileStmt: WhileStatement =>
          /* Only consider while-statements if the condition is always true. */
          whileStmt.condition match {
            case BooleanLiteralExpression(true, _) =>
              val branch = new Branch
              parent.sub += branch
              constructTree(sem, branch, whileStmt.statements)
            case _ =>
          }

        case s: ReturnStatement =>
          parent.terminates = true

        case s: ThrowStatement =>
          parent.terminates = true

        case _ =>
      }
    }

    if (!parent.terminates && parent.sub.size != 0) {
      /* If all sub-branches terminate, the parent as well. */
      for (branch <- parent.sub) {
        if (!branch.terminates) {
          return
        }
      }

      parent.terminates = true
    }
  }

  /**
   * Internally constructs a branch tree and determines whether all branches
   * terminate.
   *
   * This is done by iterating recursively over the sub-trees and investigating
   * the existence of return and throw statements. We also consider if such a
   * statement is conditional.
   *
   * Finally, using back-propagation we determine whether each sub-tree is
   * terminating. The method returns true if the root node terminates.
   */
  def terminates(sem: SemanticAnalysis, block: Block): Boolean = {
    val root = new Branch
    constructTree(sem, root, block)
    return root.terminates
  }
}