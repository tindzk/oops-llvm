package org.oopsc

import sys.process._
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNotNull
import org.junit.Assert.fail
import java.io._
import java.nio.ByteBuffer
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets
import java.nio.file.{NoSuchFileException, Files, Paths}
import java.util.Collection
import org.antlr.v4.runtime.ANTLRInputStream
import org.antlr.v4.runtime.CommonTokenStream
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.junit.runners.Parameterized.Parameters
import com.typesafe.scalalogging.slf4j.LazyLogging

@RunWith(value = classOf[Parameterized])
class TestSuite(var path: String) extends LazyLogging {
  @Test
  def testGrammar {
    val stream = new FileInputStream(this.path)
    val input = new ANTLRInputStream(stream)
    val lexer = new GrammarLexer(input)
    val tokens = new CommonTokenStream(lexer)
    val parser = new GrammarParser(tokens)

    parser.program

    val supposedToFail = this.path.contains("_se")

    if (supposedToFail) {
      assertNotNull(parser.getNumberOfSyntaxErrors)
    } else {
      assertEquals(0, parser.getNumberOfSyntaxErrors)
    }
  }

  /**
   * Performs syntax as well as contextual analysis. Also tests the code generation.
   */
  def testEverything(optim: Boolean) {
    val supposedToFail = this.path.contains("_se")
    val pathExpected = this.path.substring(0, this.path.length - 5) + ".out"
    val expected =
       try {
         TestSuite.readFile(pathExpected, StandardCharsets.UTF_8)
       } catch {
         case e: NoSuchFileException => ""
       }

    try {
      val p = new SyntaxAnalysis(this.path, false).parse

      p.semanticAnalysis
      p.printTree

      if (optim) {
        p.optimise
        p.printTree
      }

      /* Code generation. */
      val code = new CodeContext()
      p.generateCode(code)

      code.dump()
      code.verify()
      code.writeBitcode("/tmp/oops-llvm.bc")

      /* Run the LLVM interpreter. */
      val output = ("lli /tmp/oops-llvm.bc" !!)
      assertEquals(expected.trim(), output.trim())
    } catch {
      case e: CompileException => {
        if (supposedToFail) {
          logger.error(e.getMessage)
          return
        }

        throw e
      }
    }

    if (supposedToFail) {
      fail
    }
  }

  @Test
  def testDefault {
    testEverything(false)
  }

  @Test
  def testOptimisations {
    testEverything(true)
  }
}

object TestSuite {
  def readFile(path: String, encoding: Charset): String = {
    val encoded = Files.readAllBytes(Paths.get(path))
    return encoding.decode(ByteBuffer.wrap(encoded)).toString
  }

  def recursiveListFiles(f: File): Array[File] = {
    val these = f.listFiles
    these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }

  @Parameters(name = "{0}")
  def data: Collection[Array[AnyRef]] = {
    val files = recursiveListFiles(new File("tests/")).map(_.toString).filter(_.endsWith(".oops")).sorted.map(Array[AnyRef](_))
    return scala.collection.JavaConversions.mutableSeqAsJavaList(files)
  }
}