package org.oopsc

import org.llvm.{TypeRef, Builder, Value, Module}
import org.llvm.binding.LLVMLibrary
import scala.collection.JavaConverters._
import org.llvm.binding.LLVMLibrary.{LLVMCallConv, LLVMLinkage}

object LLVMHelpers {
  def buildConstStringVariable(module: Module, s: Value) = {
    val str = module.addGlobal(s.typeOf(), ".str")
    str.setGlobalConstant(true)
    str.setLinkage(LLVMLibrary.LLVMLinkage.LLVMPrivateLinkage)
    str.setInitializer(s)
    str
  }

  def getNativeFunction(code: CodeContext, funType: TypeRef, funName: String) = {
    val fun = code.module.addOrInsertFunction(funName, funType)
    fun.setLinkage(LLVMLinkage.LLVMExternalLinkage)
    fun.setFunctionCallConv(LLVMCallConv.LLVMCCallConv)
    fun
  }

  def getPointer(builder: Builder, value: Value, indices: Int*) = {
    builder.buildGEP(value, indices.map(new Integer(_)).asJava)
  }

  def getConstPointer(value: Value, indices: Int*) = {
    Value.constGEP(value, indices.map(new Integer(_)).asJava)
  }

  def getLLVMString(sem: SemanticAnalysis, module: Module, str: String) : Value = {
    val arr = Value.constString(str, str.length, true)

    val ptr = module.addGlobal(arr.typeOf(), ".strData")
    ptr.setGlobalConstant(true)
    ptr.setLinkage(LLVMLibrary.LLVMLinkage.LLVMPrivateLinkage)
    ptr.setInitializer(arr)

    Value.constNamedStruct(
      sem.types.stringWrapper.getLLVMType(),
      TypeRef.int32Type().constInt(str.length, false),
      LLVMHelpers.getConstPointer(ptr, 0, 0))
  }

  def getCString(code: CodeContext, value: String) =
    LLVMHelpers.getPointer(code.builder,
      this.buildConstStringVariable(code.module,
        Value.constString(value)), 0, 0)
}