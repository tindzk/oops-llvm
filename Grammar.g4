/* See also http://media.pragprog.com/titles/tpantlr2/code/tour/Java.g4 */
grammar Grammar;

NOT: 'NOT';
AND: 'AND';
OR:  'OR';
MOD: 'MOD';
MUL: '*';
DIV: '/';
ADD: '+';
SUB: '-';
LEQ: '<=';
GEQ: '>=';
LT:  '<';
GT:  '>';
EQ:  '=';
NEQ: '#';
TRUE: 'TRUE';
FALSE: 'FALSE';
PRIVATE: 'PRIVATE';
PROTECTED: 'PROTECTED';
PUBLIC: 'PUBLIC';

program
  : classDeclaration*;

accessLevel
  : PRIVATE
  | PROTECTED
  | PUBLIC
  ;

classDeclaration
  : 'CLASS' name=Identifier
    ('EXTENDS' extendsClass=Identifier)?
    'IS'
    memberDeclaration*
    'END CLASS'
  ;

memberDeclaration
  : memberVariableDeclaration ';'
  | methodDeclaration
  ;

memberVariableDeclaration
  : accessLevel? variableDeclaration
  ;

methodDeclaration
  : accessLevel? 'METHOD' name=Identifier
    ('(' variableDeclaration (';' variableDeclaration)* ')')?
    (':' type)?
    'IS' methodBody
  ;

methodBody
  : (variableDeclaration ';')*
    'BEGIN' statements
    'END METHOD'
  ;

variableDeclaration
  : Identifier (',' Identifier)* ':' type
  ;

type
  : Identifier
  ;

literals
  : literal (',' literal)*
  ;

statements
  : statement*
  ;

statement
  : 'IF' expression 'THEN' statements
    ('ELSEIF' expression 'THEN' statements)*
    ('ELSE' statements)?
    'END IF'                       # ifStatement
  | 'TRY' statements
    ('CATCH' literals 'DO' statements)+
    'END TRY'                      # tryStatement
  | 'WHILE' expression
    'DO' statements
    'END WHILE'                    # whileStatement
  | 'RETURN' expression? ';'       # returnStatement
  | 'THROW' expression ';'         # throwStatement
  | expression ':=' expression ';' # assignStatement
  | expression ';'                 # expressionStatement
  ;

expression
  : '(' expression ')'                              # bracketsExpression
  | literal                                         # literalExpression
  | 'SELF'                                          # selfExpression
  | 'BASE'                                          # baseExpression
  | 'NEW' Identifier                                # instantiateExpression
  | Identifier arguments?                           # memberAccessExpression
  | expression '.' Identifier arguments?            # memberAccess2Expression
  | (SUB | NOT) expression                          # unaryExpression
  | expression op=(MUL | DIV | MOD) expression      # opExpression
  | expression op=(ADD | SUB) expression            # opExpression
  | expression op=(LEQ | GEQ | LT | GT) expression  # opExpression
  | expression
    (op=EQ<assoc=right> | op=NEQ<assoc=right>)
    expression                                      # opExpression
  | expression op=AND expression                    # opExpression
  | expression op=OR expression                     # opExpression
  | expression 'ISA' Identifier                     # typeCheckExpression
  ;

arguments
  : '(' (expression (',' expression)*)? ')'
  ;

literal
  : IntegerLiteral       # integerLiteral
  | CharacterLiteral     # characterLiteral
  | StringLiteral        # stringLiteral
  | value=(TRUE | FALSE) # booleanLiteral
  | 'NULL'               # nullLiteral
  ;

Identifier
  : LETTER ALPHANUM*
  ;

IntegerLiteral
  : DIGIT+
  ;

CharacterLiteral
  : '\'' (EscapeSequence | .?) '\''
  ;

StringLiteral
  : '\'' (EscapeSequence | ~('\\'|'\''))* '\''
  ;

fragment ALPHANUM
  : LETTER | DIGIT
  ;

fragment DIGIT
  : [0-9]
  ;

fragment LETTER
  : [a-zA-Z]
  ;

fragment
EscapeSequence
  : '\\' ('b'|'t'|'n'|'f'|'r'|'\"'|'\''|'\\')
  ;

/* Match anything between { and }. */
COMMENT
  : '{' .*? '}' -> channel(HIDDEN)
  ;

LINE_COMMENT
  : '|' ~[\r\n]* '\r'? '\n' -> channel(HIDDEN)
  ;

/* Toss out whitespaces and newlines. */
WS
  : [ \t\n]+ -> skip
  ;