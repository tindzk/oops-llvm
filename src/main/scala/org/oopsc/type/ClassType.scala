package org.oopsc.symbol

import scala.collection.mutable.{ ListBuffer, LinkedHashMap }
import scala.collection.JavaConverters._
import scala.collection.JavaConversions._

import org.oopsc._
import org.oopsc.scope.Scope
import org.llvm.{TypeRef, Value}
import org.oopsc.expression.BinaryExpression
import org.llvm.binding.LLVMLibrary

object ClassType {
  /**
   * Constant for the size of the header at the beginning of each object.
   * As of now, the header only contains the address to the object's VMT.
   */
  final val HeaderSize = 1

  /**
   * The VMT attributes start with the offset 1. The first entry in the VMT
   * is reserved for the base class.
   */
  final val VirtualMethodTableOffset = 1
}

class ClassType(identifier: Identifier) extends ScopedType {
  var attributes = new RecordType(identifier)
  var methods = new TypeWrapSymbol(identifier)

  /** List of all fields and methods. */
  var members = new LinkedHashMap[String, Type]()

  /** Parent class. */
  var superClass: Option[ResolvableType] = None

  /**
   * The size of an object described by this class. The exact size will be determined
   * during the referential pass. The minimum size for all objects is ClassSymbol.HeaderSize.
   */
  var objectSize = -1

  def this(ident: Identifier, superClass: ClassType) {
    this(ident)
    this.superClass = Some(new ResolvableType(superClass.ident()))
  }

  def this(ident: Identifier, superClass: ResolvableType) {
    this(ident)
    this.superClass = Some(superClass)
  }

  override def ident() = identifier
  override def op(op: BinaryExpression.Operator): Option[OperatorSymbol] =
    this.methods.op(op)

  /**
   * Finds the declaration for the given attribute name.
   */
  private def getAttribute(name: String) =
    this.attributes.attributes.find(_.name() == name)

  /**
   * Finds the declaration for the given method name.
   */
  private def getMethod(name: String) =
    this.methods.operations.find(_.name() == name)

  private def collectAttributes(): List[FieldSymbol] = {
    (this.attributes.attributes ++ (this.superClass match {
      case Some(c) => c.declaration.get.asInstanceOf[ClassType].collectAttributes()
      case None => List[FieldSymbol]()
    })).toList
  }

  /**
   * Recursively collect all methods in the inheritance chain. Filter out
   * overridden methods.
   */
  /* TODO Delete `overrides' attribute from MethodSymbol. Find a better way to solve this. */
  private def collectMethods(overridden: List[MethodSymbol] = List.empty): ListBuffer[MethodSymbol] = {
    val _overridden = overridden ++ this.methods.operations.filter(_.overrides.isDefined).map(_.overrides.get)
    this.methods.operations ++ (this.superClass match {
      case Some(c) => c.declaration.get.asInstanceOf[ClassType].collectMethods(_overridden).diff(_overridden)
      case None => ListBuffer[MethodSymbol]()
    })
  }

  /**
   * Generates a VMT for the current class, including its sub-classes. Requires
   * that the contextual analysis was previously performed.
   */
  def generateVMT = this.collectMethods().sortBy(_.vmtIndex)

  override def defPass(sem: SemanticAnalysis) {
    sem.defineSymbol(this)
    sem.enter(this)

    if (this.superClass.isEmpty && !(this eq sem.types.objectClass)) {
      /* Object is the only class without a super class. */
      this.superClass = {
        val c = new ResolvableType(sem.types.objectClass.ident())
        c.declaration = Some(sem.types.objectClass)
        Some(c)
      }
    }

    this.attributes.attributes.foreach(_.defPass(sem))
    this.methods.operations.foreach(_.defPass(sem))

    sem.leave
  }

  def getSuperClass(): Option[Type] = {
    this.superClass match {
      case Some(superClass) =>
        if (superClass.declaration.isEmpty) {
          val base = this.resolveClass(this.sem, superClass.identifier)
          superClass.declaration = Some(base)
        }

        superClass.declaration

      case None => None
    }
  }

  /**
   * Check whether the class dependencies represent an acyclic graph. This is
   * done by traversing the class hierarchy recursively. If a class occurs more
   * than once, a cycle was found.
   */
  def checkForCycles(encounteredClasses: List[ClassType] = List.empty) {
    this.superClass match {
      case Some(superClass) =>
        val base = getSuperClass().get

        if (encounteredClasses.contains(base)) {
          throw new CompileException("Class hierarchy is not devoid of cycles.", this.pos())
        }

        base.asInstanceOf[ClassType].checkForCycles(this :: encounteredClasses)

      case None =>
    }
  }

  var collectedAttrs : List[FieldSymbol] = null

  private var sem: SemanticAnalysis = null
  override def refPass(sem: SemanticAnalysis) {
    this.sem = sem
    sem.enter(this)

    this.checkForCycles()

    this.superClass match {
      case None =>
        /* Set the VMT index for each method. */
        for ((m, i) <- this.methods.operations.zipWithIndex) {
          m.vmtIndex = i + ClassType.VirtualMethodTableOffset
        }

      case Some(superClass) =>
        val base = getSuperClass().get.asInstanceOf[ClassType]

        /* Verify that all overridden methods have the same signature as its parent. */
        for (m <- this.methods.operations) {
          base.getMethod(m.name()) match {
            case Some(baseMethod) =>
              /* This method overrides a parent method. */
              if (!baseMethod.signatureEquals(m)) {
                throw new CompileException(
                  s"The overridden signature of ${this.name()}.${m.name()}() does not match its parent method in ${base.name()}.", m.pos())
              }

            case None =>
          }

          if (base.getAttribute(m.name()).isDefined) {
            throw new CompileException(
              s"The method ${this.name()}.${m.name()}() is overriding a method of its base class ${base.name()}.", m.pos())
          }
        }

        for (v <- this.attributes.attributes) {
          if (base.getMethod(v.name()).isDefined) {
            throw new CompileException(
              s"The attribute ${v.name()} in ${this.name()} is overriding a method of its base class ${base.name()}.", v.pos())
          }
        }

        /* Set the VMT index for each method. Note that the VMT does not contain operator methods as these are always
         * inlined.
         */
        var vmtIndex =
          if (base.methods.operations.isEmpty) ClassType.VirtualMethodTableOffset
          else base.methods.operations.last.vmtIndex + 1 /* TODO Perhaps we should store the highest index in the parent? */

        for (m <- this.methods.operations) {
          /* If the method is overridden, take the VMT index from its parent method. */
          base.getMethod(m.name()) match {
            case Some(baseMethod) =>
              m.vmtIndex = baseMethod.vmtIndex
              m.overrides = Some(baseMethod)

            case None =>
              m.vmtIndex = vmtIndex
              vmtIndex += 1
          }
        }
    }

    /* Inherit attributes from the parent object. */
    this.collectedAttrs = this.collectAttributes()
    this.objectSize = ClassType.HeaderSize + this.collectedAttrs.size

    /* Resolve attribute types and assign indices. */
    for ((a, i) <- this.attributes.attributes.reverse.zipWithIndex) {
      a.refPass(sem)
      a.offset = this.objectSize - 1 - i
    }

    this.methods.operations.foreach(_.refPass(sem))

    sem.leave
  }

  override def optimPass() {
    this.methods.operations.foreach(_.optimPass())
  }

  var typeInfo : Value = null
  private var llvmVTable : Value = null
  private var llvmType : TypeRef = null

  def getVirtualMethodTable(code: CodeContext) : Value = {
    if (this.llvmVTable == null) {
      val methods = this.generateVMT

      val vtableEntryType = TypeRef.int64Type().pointerType()

      /* The first entry of the VMT is a reference to the VMT of the super class. */
      val superClassReference =
        if (this.getSuperClass() == None) {
          /* For classes without super class, take a NULL pointer. */
          TypeRef.int32Type().constInt(0, false)
        } else {
          this.getSuperClass().get.asInstanceOf[ClassType].getVirtualMethodTable(code)
        }

      val vtableEntries = List(Value.constBitCast(superClassReference, vtableEntryType)) ++
        methods.map(m => Value.constBitCast(m.getLLVMFunction(), vtableEntryType))

      val vtableType = vtableEntryType.arrayType(vtableEntries.length)
      val vtable = code.module.addGlobal(vtableType, this.name() + ".VTable")
      vtable.setGlobalConstant(true)
      vtable.setLinkage(LLVMLibrary.LLVMLinkage.LLVMPrivateLinkage)
      vtable.setInitializer(Value.constArray(vtableEntryType, vtableEntries.asJava))

      this.llvmVTable = vtable
    }

    this.llvmVTable
  }

  override def llvmDeclPass(code: CodeContext) {
    this.methods.operations.foreach(_.llvmDeclPass(code))

    /* Create recursively all virtual method tables for this class and its super classes. */
    this.getVirtualMethodTable(code)
  }

  /**
   * Generates assembly code for this class. Requires prior completion of the
   * contextual analysis.
   */
  override def generateCode(code: CodeContext) {
    this.methods.operations.foreach(_.generateCode(code))
  }

  override def print(tree: TreeStream) {
    tree.println("CLASS " + this.name())
    tree.indent

    if (!this.attributes.attributes.isEmpty) {
      tree.println("ATTRIBUTES")
      tree.indent
      this.attributes.attributes.foreach(_.print(tree))
      tree.unindent
    }

    if (!this.methods.operations.isEmpty) {
      tree.println("METHODS")
      tree.indent
      this.methods.operations.foreach(_.print(tree))
      tree.unindent
    }

    tree.unindent
  }

  override def getParentScope(): Option[Scope] =
    superClass match {
      case Some(c) =>
        c.declaration match {
          case Some(s) => Some(s.asInstanceOf[Scope])
          case None => enclosingScope
        }

      case None => enclosingScope
    }

  /** For access such as a.b, only look in a's class hierarchy to resolve b. */
  def resolveMember(name: String): Option[Type] = {
    members.get(name) match {
      case Some(m) => return Some(m)
      case None => None
    }

    /* If not in this class, check the superclass chain. */
    superClass match {
      case Some(c) =>
        c.declaration match {
          case Some(s) => s.asInstanceOf[ClassType].resolveMember(name)
          case None => None
        }

      case None => None
    }
  }

  override protected def resolve(sem: SemanticAnalysis, ident: Identifier, requestingClass: Option[ScopedType]): Option[Type] =
    resolveMember(ident.name) match {
      case Some(m) => Some(m)
      case None => super.resolve(sem, ident, requestingClass)
    }

  override def instantiate(code: CodeContext, arguments: Value*) : Value = {
    /* TODO Figure out when it is sufficient to allocate the object on the stack. */
    val inst = code.builder.buildMalloc(this.getLLVMType(), this.name())

    /* Insert the address pointing to the VMT at the relative position 0 of the
     * object. The offsets 1.. denote the attributes. */
    val refVTable = LLVMHelpers.getPointer(code.builder, inst, 0, 0)
    code.builder.buildStore(LLVMHelpers.getPointer(code.builder, this.getVirtualMethodTable(code), 0, 0), refVTable)

    if (arguments.length > 0) {
      /* TODO For more parameters, don't use assign but call constructor. */
      this.assign(code, inst, arguments(0))
    }

    inst
  }

  override def getLLVMType(): TypeRef = {
    if (llvmType == null) {
      llvmType = TypeRef.structTypeNamed(this.name())

      /* The first element in each object points to its VMT. */
      val vmt = List(TypeRef.int64Type().pointerType().pointerType())
      TypeRef.structSetBody(llvmType, vmt ++ this.collectedAttrs.map(_.getLLVMType()), false)
    }

    this.llvmType
  }
}