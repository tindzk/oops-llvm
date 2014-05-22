package org.oopsc

import com.typesafe.scalalogging.slf4j.LazyLogging
import org.rogach.scallop._

class Conf(args : Seq[String]) extends ScallopConf(args) {
  version(s"oopsc ${OOPSC.Version} (c) 2014 Monty Team")
  banner("""Usage: java -jar oopsc.jar [OPTION]... [input] [<output>]
           |oopsc is an OOPS compiler.
           |
           |Options:
           |""".stripMargin)

  val symbols = opt[Boolean](descr = "show symbols from the syntax analysis")
  val ast = opt[Boolean](descr = "print AST after contextual analysis")
  val debug = opt[Boolean](descr = "enable debug mode")
  val generateCode = toggle(name = "code", descrYes = "enable code generation (default)", descrNo = "disable code generation", default = Some(true))
  val run = toggle(descrYes = "execute code in JIT (default)", descrNo = "disable execution", default = Some(true))
  val help = opt[Boolean](descr = "print help")
  val optimisations = opt[Boolean]("optim", descr = "enable optimisations")
  val inputFile = trailArg[String]("input", descr = "input file")
  val outputFile = trailArg[String]("output", descr = "path for saving LLVM bitcode", required = false)
}

object OOPSC extends LazyLogging {
  val Version = "0.1"

  def main(args: Array[String]) {
    val conf = new Conf(args)

    if (conf.help.apply()) {
      conf.printHelp()
      return
    }

    try {
      val p = new SyntaxAnalysis(conf.inputFile.apply(), conf.symbols.apply()).parse

      p.semanticAnalysis

      if (conf.optimisations.apply()) {
        p.optimise
      }

      if (conf.ast.apply()) {
        p.printTree
      }

      val code = new CodeContext(conf.run.apply())

      if (conf.generateCode.apply()) {
        p.generateCode(code)

        if (conf.outputFile.isDefined) {
          code.writeBitcode(conf.outputFile.get.get)
        }

        if (conf.run.apply()) {
          code.run()
        }
      }
    } catch {
      case e: CompileException => {
        logger.error(e.getMessage)

        if (conf.debug.apply()) {
          e.printStackTrace
        }

        System.exit(1)
      }
    }
  }
}