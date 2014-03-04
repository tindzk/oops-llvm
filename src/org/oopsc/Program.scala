package org.oopsc

import org.oopsc.expression._
import org.oopsc.scope._
import org.oopsc.symbol._
import org.oopsc.statement._
import scala.collection.mutable.ListBuffer
import org.llvm.TypeRef
import org.llvm.binding.LLVMLibrary.LLVMCallConv

/**
 * Represents the syntax tree of the entire program. Entry point for contextual
 * analysis and code generation.
 */
class Program {
  var sem = new SemanticAnalysis

  /**
   * User-defined classes.
   */
  var types = new ListBuffer[Type]

  /**
   * Initialisation statements.
   */
  var init = new ListBuffer[Statement]

  /**
   * Add a statement that instantiates the class `Main` and calls its method main().
   * Equivalent to NEW Main.main.
   */
  this.init += new CallStatement(new AccessExpression(new NewExpression(new ResolvableType(new Identifier("Main"))), new EvaluateExpression(new ResolvableType(new Identifier("main")))))

  /**
   * Defines custom class.
   */
  def addType(t: Type) {
    this.types += t
  }

  /**
   * Performs the semantic analysis for the entire program.
   */
  def semanticAnalysis {
    /* Add built-in classes. */
    this.types ++= List(
      this.sem.types.boolWrapper,
      this.sem.types.byteWrapper,
      this.sem.types.charWrapper,
      this.sem.types.hexWrapper,
      this.sem.types.intWrapper,
      this.sem.types.objectClass,
      this.sem.types.octWrapper,
      this.sem.types.stringWrapper,
      this.sem.types.strStruct)

    val scope = new GlobalScope
    this.sem.enter(scope)

    this.types.foreach(_.defPass(this.sem))
    this.types.foreach(_.refPass(this.sem))

    /* Resolve dependencies for startup statements. */
    this.init.foreach(_.refPass(this.sem))

    this.sem.leave

    if (this.sem.currentScope.isDefined) {
      throw new CompileException("Current scope must be `None' after semantic analysis.")
    }
  }

  /**
   * Optimisation pass for all classes.
   */
  def optimise {
    this.types.foreach(_.optimPass())
  }

  /**
   * Prints abstract syntax tree (AST).
   */
  def printTree {
    val tree = new TreeStream(System.out, 4)
    this.types.foreach(_.print(tree))
  }

  /**
   * Code generation.
   */
  // TODO llvmGenPass
  def generateCode(code: CodeContext) {
    code.sem = sem

    /* TODO Store console arguments. */
    import scala.collection.JavaConverters._
    val func = code.module.addFunction("main", TypeRef.functionType(TypeRef.int32Type(), List[TypeRef]().asJava))
    func.setFunctionCallConv(LLVMCallConv.LLVMCCallConv)

    this.types.foreach(_.llvmDeclPass(code))

    val firstBodyBlock = func.appendBasicBlock("body")
    code.builder.positionBuilderAtEnd(firstBodyBlock)

    /* Generate code for initialisation statements. */
    this.init.foreach(_.generateCode(code))

    /* Return exit code for `success' in main(). */
    code.builder.buildRet(TypeRef.int32Type().constInt(0, false))

    /* Generate code for user-defined classes. */
    this.types.foreach(_.generateCode(code))
  }
}