package org.oopsc.symbol

import org.oopsc._
import org.oopsc.statement._
import scala.collection.mutable.ListBuffer
import org.llvm._
import org.llvm.binding.LLVMLibrary._
import scala.collection.JavaConverters._

class MethodSymbol(identifier: Identifier) extends ScopedType {
  /** Local variable SELF. */
  var self: SelfVariableSymbol = null

  /** Local variable BASE. */
  var base: VariableSymbol = null

  /** List of all parameters. */
  var parameters = new ListBuffer[ParameterSymbol]()

  /** The method's local variables. */
  var locals = new ListBuffer[VariableSymbol]

  /** The method body, i.e. its statements. */
  var statements = new Block

  /** Return type. Default type corresponds to `Void`. */
  var retType: Identifier = null
  var resolvedRetType: Option[Type] = None

  var vmtIndex = -1
  var overrides: Option[MethodSymbol] = None

  private var llvmFunction : Value = null

  override def ident() = identifier
  override def getLLVMType() = null // TODO not implemented yet

  def getResolvedReturnType(sem: SemanticAnalysis): Type = {
    /* Resolve return type if necessary. */
    if (this.resolvedRetType.isEmpty) {
      this.resolvedRetType = Some(this.resolveClass(sem, this.retType))
    }

    this.resolvedRetType.get
  }

  def resolveParameter(name: String) =
    parameters.find(_.name() == name)

  override protected def resolve(sem: SemanticAnalysis, ident: Identifier, requestingClass: Option[ScopedType]): Option[Type] =
    this.resolveParameter(ident.name) match {
      case Some(m) => Some(m)
      case None => super.resolve(sem, ident, requestingClass)
    }

  override def defPass(sem: SemanticAnalysis) {
    sem.defineSymbol(this)
    sem.enter(this)

    if (this.retType == null) {
      this.resolvedRetType = new Some(Types.voidType)
    }

    if ((sem.currentScopedType.name == "Main") && (this.name() == "main")) {
      if (this.parameters.size != 0) {
        throw new CompileException("Main.main() must not have any parameters.", this.pos())
      } else if (this.getResolvedReturnType(this.sem) ne Types.voidType) {
        throw new CompileException("Main.main() must not have a non-void return type.", this.pos())
      }
    }

    /* BASE represents the inherited class. Define type for all classes except for Object. */
    sem.currentScopedType match {
      case t: TypeWrapSymbol => {
        /* Register variable SELF pointing to the class object. */
        this.self = new SelfVariableSymbol(t)
        this.defineSymbol(this.self)
      }

      case c: ClassType => {
        this.self = new SelfVariableSymbol(c)
        this.defineSymbol(this.self)

        c.getSuperClass match {
          case Some(superClass) =>
            this.base = new VariableSymbol(new Identifier("BASE"), superClass)
            this.base.accessLevel = AccessLevel.Private
            this.base.declaringContext = Some(sem.currentScopedType)
            this.defineSymbol(this.base)

          case None =>
        }
      }

      case _ =>
    }

    /* Register all parameters. */
    this.parameters.foreach(_.defPass(sem))

    /* Register all local variables. */
    this.locals.foreach(_.defPass(sem))

    sem.leave()
  }

  var sem: SemanticAnalysis = null

  override def refPass(sem: SemanticAnalysis) {
    this.sem = sem
    sem.enter(this)

    this.overrides match {
      case Some(m) =>
        /* TODO Can private methods be overwritten by subclasses? */
        if (m.accessLevel != this.accessLevel) {
          throw new CompileException(
            s"${this.name()} overwrites method in superclass ${sem.currentScopedType.name()} with different access level.",
            this.pos())
        }

      case _ =>
    }

    /* Resolve types of all parameters and variables. */
    this.parameters.foreach(_.refPass(sem))
    this.locals.foreach(_.refPass(sem))

    /* IDs start with 1 as 0 points to the class object. */
    for ((p, i) <- this.parameters.zipWithIndex) {
      p.id = i + 1
    }

    val hasReturnValue = this.getResolvedReturnType(this.sem) ne Types.voidType
    val terminates = BranchEvaluator.terminates(sem, this.statements)

    if (hasReturnValue && !terminates) {
      throw new CompileException("Method needs a reachable returning statement.", this.retType.position)
    }

    /* Reference pass for all statements. */
    this.statements.refPass(sem)

    sem.leave()
  }

  override def optimPass() {
    this.statements.optimPass()
  }

  def print(tree: TreeStream) {
    tree.println(s"${this.accessLevel} METHOD ${this.name()} (${this.vmtIndex}): " +
      this.resolvedRetType.map(_.name()).getOrElse("<unresolved>"))
    tree.indent

    if (!this.parameters.isEmpty) {
      tree.println("PARAMETERS")
      tree.indent
      this.parameters.foreach(_.print(tree))
      tree.unindent
    }

    if (!this.locals.isEmpty) {
      tree.println("VARIABLES")
      tree.indent
      this.locals.foreach(_.print(tree))
      tree.unindent
    }

    tree.println("BEGIN")
    this.statements.print(tree)
    tree.println("END")

    tree.unindent
  }

  /**
   * Returns the function name as it is used in the LLVM bitcode. Include the declaring class
   * to avoid collisions.
   */
  def getLLVMName() =
    this.declaringContext.get.name() + "." + this.name()

  def getLLVMReturnType() = {
    val t = this.getResolvedReturnType(this.sem)

    if (t.mustEmbed()) {
      t.getLLVMType()
    } else {
      t.getLLVMType().pointerType()
    }
  }

  def getLLVMFunction() = llvmFunction

  def getLLVMFunctionType() = {
    val ret = this.getLLVMReturnType()
    val params = this.parameters.map(_.getResolvedType.getLLVMType())
    TypeRef.functionType(ret, params.asJava)
  }

  def getLLVMFunctionType(context: Type) = {
    val ret = this.getLLVMReturnType()
    val params = List(context.getLLVMType().pointerType()) ++ this.parameters.map(_.getResolvedType.getLLVMType())
    TypeRef.functionType(ret, params.asJava)
  }

  override def llvmDeclPass(code: CodeContext) {
    val llvmName = this.getLLVMName()

    val llvmFunctionType =
      if (this.self == null) {
        /* For non-class methods. */
        getLLVMFunctionType()
      } else {
        val self = this.self.getResolvedType
        getLLVMFunctionType(self)
      }

    code.currentFunction = code.module.addFunction(llvmName, llvmFunctionType)
    code.currentFunction.setFunctionCallConv(LLVMCallConv.LLVMCCallConv)

    if (this.self != null) {
      code.currentFunction.getParam(0).setValueName("self")
    }

    for (i <- 1 to code.currentFunction.countParams() - 1) {
      code.currentFunction.getParam(i).setValueName(this.parameters(i - 1).name())
    }

    llvmFunction = code.currentFunction
  }

  override def generateCode(code: CodeContext) {
    code.currentFunction = llvmFunction

    val bodyBlock = code.currentFunction.appendBasicBlock("body")

    /* TODO Only generate this block if there is more than one return statement. Otherwise, it's not needed. */
    code.retBlock = code.currentFunction.appendBasicBlock("return")

    code.builder.positionBuilderAtEnd(bodyBlock)

    if (this.resolvedRetType != Some(Types.voidType)) {
     code.retValue = code.builder.buildAlloca(this.getLLVMReturnType(), "retval")
    }

    this.statements.generateCode(code, code.retBlock)

    code.builder.positionBuilderAtEnd(code.retBlock)

    if (this.resolvedRetType == Some(Types.voidType)) {
      code.builder.buildRetVoid()
    } else {
      code.builder.buildRet(code.builder.buildLoad(code.retValue, ""))
    }
  }

  /**
   * Compares the signature for equality.
   *
   * @param m Comparison method.
   * @return true if signatures are equal, false otherwise.
   */
  def signatureEquals(m: MethodSymbol): Boolean = {
    if (this.getResolvedReturnType(this.sem) ne m.getResolvedReturnType(this.sem)) {
      return false
    }

    if (m.parameters.size != this.parameters.size) {
      return false
    }

    for ((left, right) <- this.parameters.zip(m.parameters)) {
      if (left.getResolvedType ne right.getResolvedType) {
        return false
      }
    }

    true
  }
}