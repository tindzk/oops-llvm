package org.oopsc

import java.io.FileInputStream
import java.util.Collections
import org.antlr.v4.runtime._
import org.oopsc.symbol._
import scala.collection.mutable.ListBuffer
import org.oopsc.expression._
import org.oopsc.statement._
import org.oopsc.symbol.AccessLevel.AccessLevel
import scala.collection.JavaConversions._

class CustomErrorListener(var syntax: SyntaxAnalysis) extends BaseErrorListener {
  override def syntaxError(recognizer: Recognizer[_, _], offendingSymbol: AnyRef, line: Int, charPositionInLine: Int, msg: String, e: RecognitionException) {
    val stack = recognizer.asInstanceOf[Parser].getRuleInvocationStack
    Collections.reverse(stack)

    val message = s"$msg with rule stack $stack"

    val offendingToken = offendingSymbol.asInstanceOf[Token]
    val tokens = recognizer.getInputStream.asInstanceOf[CommonTokenStream]
    val input = tokens.getTokenSource.getInputStream.toString

    /* Collect information for underlining the error. */
    val errorLine = input.split("\n")(line - 1)
    val start = offendingToken.getStartIndex
    val stop = offendingToken.getStopIndex

    throw new CompileException(message, new Position(line, charPositionInLine), errorLine, start, stop)
  }
}

/* Performs syntactic analysis using the ANTLR4 grammar for the OOPS language.
 * Then constructs abstract syntax tree (AST).
 */
class SyntaxAnalysis(fileName: String, var printSymbols: Boolean) {
  private final val file = new FileInputStream(fileName)

  private def identifierFromToken(t: Token): Identifier =
    new Identifier(t.getText, new Position(t.getLine, t.getCharPositionInLine))

  private def resolvableIdentifierFromToken(t: Token): ResolvableType =
    new ResolvableType(new Identifier(t.getText, new Position(t.getLine, t.getCharPositionInLine)))

  private def program(ctx: GrammarParser.ProgramContext, p: Program) {
    for (c <- ctx.classDeclaration()) {
      p.addType(this.getClassDeclaration(c))
    }
  }

  private def getClassDeclaration(ctx: GrammarParser.ClassDeclarationContext): ClassType = {
    val c = ctx.extendsClass match {
      case null =>
        new ClassType(this.identifierFromToken(ctx.name))
      case e =>
        new ClassType(this.identifierFromToken(ctx.name), this.resolvableIdentifierFromToken(e))
    }

    for (m <- ctx.memberDeclaration()) {
      if (m.memberVariableDeclaration() != null) {
        memberVariableDeclaration(m.memberVariableDeclaration(), c)
      } else if (m.methodDeclaration() != null) {
        methodDeclaration(m.methodDeclaration(), c)
      }
    }

    c
  }

  private def variableDeclaration(ctx: GrammarParser.VariableDeclarationContext, vars: ListBuffer[VariableSymbol]) {
    val `type` = this.identifierFromToken(ctx.`type`.start)

    for (ident <- ctx.Identifier) {
      val name = this.identifierFromToken(ident.getSymbol)
      vars += new VariableSymbol(name, `type`)
    }
  }

  private def parameterDeclaration(ctx: GrammarParser.VariableDeclarationContext, vars: ListBuffer[ParameterSymbol]) {
    val `type` = this.identifierFromToken(ctx.`type`.start)

    for (ident <- ctx.Identifier) {
      val name = this.identifierFromToken(ident.getSymbol)
      vars += new ParameterSymbol(name, `type`)
    }
  }

  private def attributeDeclaration(ctx: GrammarParser.VariableDeclarationContext, vars: RecordType, accessLevel: AccessLevel) {
    val `type` = this.identifierFromToken(ctx.`type`.start)

    for (ident <- ctx.Identifier) {
      val name = this.identifierFromToken(ident.getSymbol)
      val sym = new FieldSymbol(name, `type`)
      sym.accessLevel = accessLevel
      vars.attributes += sym
    }
  }

  private def accessLevel(ctx: GrammarParser.AccessLevelContext): AccessLevel = {
    if (ctx != null) {
      if (ctx.PRIVATE() != null) {
        return AccessLevel.Private
      } else if (ctx.PROTECTED() != null) {
        return AccessLevel.Protected
      }
    }

    AccessLevel.Public
  }

  private def memberVariableDeclaration(ctx: GrammarParser.MemberVariableDeclarationContext, c: ClassType) {
    this.attributeDeclaration(ctx.variableDeclaration, c.attributes, this.accessLevel(ctx.accessLevel()))
  }

  private def methodBody(ctx: GrammarParser.MethodBodyContext, m: MethodSymbol) {
    for (variable <- ctx.variableDeclaration) {
      this.variableDeclaration(variable, m.locals)
    }

    m.statements = this.getStatements(ctx.statements)
  }

  private def methodDeclaration(ctx: GrammarParser.MethodDeclarationContext, c: ClassType) = {
    var m = new MethodSymbol(this.identifierFromToken(ctx.name))
    m.accessLevel = this.accessLevel(ctx.accessLevel())

    c.methods.operations += m

    for (variable <- ctx.variableDeclaration) {
      this.parameterDeclaration(variable, m.parameters)
    }

    if (ctx.`type` != null) {
      val retType = this.identifierFromToken(ctx.`type`.start)
      m.retType = retType
    }

    methodBody(ctx.methodBody(), m)
    m
  }

  private def getLiteral(ctx: GrammarParser.LiteralContext): LiteralExpression = {
    val rctx: RuleContext = ctx.getRuleContext
    val pos = new Position(ctx.start.getLine, ctx.start.getCharPositionInLine)

    rctx match {
      case l: GrammarParser.IntegerLiteralContext =>
        IntegerLiteralExpression(Integer.parseInt(ctx.getText), pos)
      case l: GrammarParser.CharacterLiteralContext =>
        val value = ctx.getText.substring(1, ctx.getText.length - 1)

        if (value == "\\n") {
          CharacterLiteralExpression('\n', pos)
        } else if (value == "\\\\") {
          CharacterLiteralExpression('\\', pos)
        } else if (value.length != 1) {
          throw new CompileException("Unsupported character in literal.", pos)
        } else {
          CharacterLiteralExpression(value.charAt(0), pos)
        }
      case l: GrammarParser.StringLiteralContext =>
        val value = ctx.getText.substring(1, ctx.getText.length - 1)
        StringLiteralExpression(value.replaceAll("\\\\'", "'"), pos)
      case l: GrammarParser.BooleanLiteralContext =>
        BooleanLiteralExpression((l.value.getType == GrammarParser.TRUE), pos)
      case l: GrammarParser.NullLiteralContext =>
        NullLiteralExpression(pos)
    }
  }

  private def getArguments(ctx: GrammarParser.ArgumentsContext, eval: EvaluateExpression) = {
    for (expr <- ctx.expression) {
      eval.addArgument(this.getExpression(expr))
    }
  }

  private def getExpression(ctx: GrammarParser.ExpressionContext): Expression = {
    val rctx: RuleContext = ctx.getRuleContext
    val pos = new Position(ctx.start.getLine, ctx.start.getCharPositionInLine)

    rctx match {
      case e: GrammarParser.BracketsExpressionContext =>
        this.getExpression(e.expression)
      case e: GrammarParser.MemberAccessExpressionContext =>
        val eval = new EvaluateExpression(this.resolvableIdentifierFromToken(e.Identifier.getSymbol))

        if (e.arguments() != null) {
          this.getArguments(e.arguments(), eval)
        }

        eval
      case e: GrammarParser.MemberAccess2ExpressionContext =>
        val eval = new EvaluateExpression(this.resolvableIdentifierFromToken(e.Identifier.getSymbol))

        if (e.arguments() != null) {
          this.getArguments(e.arguments(), eval)
        }

        new AccessExpression(this.getExpression(e.expression), eval)
      case e: GrammarParser.LiteralExpressionContext =>
        this.getLiteral(e.literal)
      case e: GrammarParser.SelfExpressionContext =>
        new EvaluateExpression(new ResolvableType(new Identifier("SELF", pos)))
      case e: GrammarParser.BaseExpressionContext =>
        new EvaluateExpression(new ResolvableType(new Identifier("BASE", pos)))
      case e: GrammarParser.UnaryExpressionContext =>
        if (e.SUB() != null) {
          new UnaryExpression(UnaryExpression.MINUS, this.getExpression(e.expression), pos)
        } else {
          new UnaryExpression(UnaryExpression.NOT, this.getExpression(e.expression), pos)
        }
      case e: GrammarParser.InstantiateExpressionContext =>
        new NewExpression(this.resolvableIdentifierFromToken(e.Identifier.getSymbol))
      case e: GrammarParser.OpExpressionContext =>
        val op = e.op.getType match {
          case GrammarParser.MUL => BinaryExpression.MUL
          case GrammarParser.DIV => BinaryExpression.DIV
          case GrammarParser.MOD => BinaryExpression.MOD
          case GrammarParser.ADD => BinaryExpression.PLUS
          case GrammarParser.SUB => BinaryExpression.MINUS
          case GrammarParser.LEQ => BinaryExpression.LTEQ
          case GrammarParser.GEQ => BinaryExpression.GTEQ
          case GrammarParser.LT  => BinaryExpression.LT
          case GrammarParser.GT  => BinaryExpression.GT
          case GrammarParser.AND => BinaryExpression.AND
          case GrammarParser.OR  => BinaryExpression.OR
          case GrammarParser.EQ  => BinaryExpression.EQ
          case GrammarParser.NEQ => BinaryExpression.NEQ
        }

        new BinaryExpression(this.getExpression(e.expression(0)), op, this.getExpression(e.expression(1)))
      case e: GrammarParser.TypeCheckExpressionContext =>
        new TypeCheckExpression(
          this.getExpression(e.expression()),
          this.resolvableIdentifierFromToken(e.Identifier.getSymbol))
    }
  }

  private def getIfStatement(ctx: GrammarParser.IfStatementContext): Statement = {
    val s = new IfStatement(this.getExpression(ctx.expression(0)), this.getStatements(ctx.statements(0)))

    var i = 1

    while (i < ctx.expression.size) {
      s.addIfElse(this.getExpression(ctx.expression(i)), this.getStatements(ctx.statements(i)))
      i += 1
    }

    if (i < ctx.statements.size) {
      s.setElse(this.getStatements(ctx.statements(i)))
    }

    s
  }

  private def getTryStatement(ctx: GrammarParser.TryStatementContext): Statement = {
    val pos = new Position(ctx.start.getLine, ctx.start.getCharPositionInLine)
    val s = new TryStatement(this.getStatements(ctx.statements(0)), pos)

    for (i <- 0 to ctx.literals().size() - 1) {
      s.addCatchBlock(this.getLiterals(ctx.literals(i)), this.getStatements(ctx.statements(i + 1)))
    }

    s
  }

  private def getStatement(ctx: GrammarParser.StatementContext): Statement = {
    val rctx: RuleContext = ctx.getRuleContext
    val pos = new Position(ctx.start.getLine, ctx.start.getCharPositionInLine)

    rctx match {
      case s: GrammarParser.IfStatementContext =>
        this.getIfStatement(s)
      case s: GrammarParser.TryStatementContext =>
        this.getTryStatement(s)
      case s: GrammarParser.WhileStatementContext =>
        new WhileStatement(this.getExpression(s.expression), this.getStatements(s.statements))
      case s: GrammarParser.ReturnStatementContext =>
        if (s.expression == null) {
          new ReturnStatement(pos)
        } else {
          new ReturnStatement(pos, this.getExpression(s.expression))
        }
      case s: GrammarParser.ThrowStatementContext =>
        new ThrowStatement(this.getExpression(s.expression), pos)
      case s: GrammarParser.AssignStatementContext =>
        new AssignStatement(this.getExpression(s.expression(0)), this.getExpression(s.expression(1)))
      case s: GrammarParser.ExpressionStatementContext =>
        new CallStatement(this.getExpression(s.expression))
    }
  }

  private def getLiterals(ctx: GrammarParser.LiteralsContext) = {
    val literals = scala.collection.JavaConversions.collectionAsScalaIterable(ctx.literal())
    literals.map(this.getLiteral(_)).to[ListBuffer]
  }

  private def getStatements(ctx: GrammarParser.StatementsContext) = {
    val stmts = scala.collection.JavaConversions.collectionAsScalaIterable(ctx.statement())
    new Block(stmts.map(this.getStatement(_)).to[ListBuffer])
  }

  def parse: Program = {
    val input = new ANTLRInputStream(this.file)
    val lexer = new GrammarLexer(input)
    val tokens = new CommonTokenStream(lexer)
    val parser = new GrammarParser(tokens)

    /* Remove ConsoleErrorListener and add our custom error listener. */
    parser.removeErrorListeners
    parser.addErrorListener(new CustomErrorListener(this))

    val tree = parser.program

    if (this.printSymbols) {
      println(tree.toStringTree(parser))
    }

    val p = new Program
    program(tree, p)
    p
  }
}