parser grammar IleParser;

options {
    tokenVocab = IleLexer;
    superClass = IleParserBase;
}

sourceFile
     :packageClause eos (importSpec eos)* ((
        functionDecl
//      |   methodDecl
         |  declaration
          ) eos)* EOF
    ;

packageClause
    : PACKAGE packageName = IDENTIFIER
    ;

importSpec
    : IMPORT alias = (DOT | IDENTIFIER)? importPath
    ;

importPath
    : string_
    ;


declaration
    : varDecl
    ;

varDecl
    : VAL IDENTIFIER (COLON type_)? ASSIGN expression
    ;

block
    : L_CURLY blockExpr? R_CURLY
    |
    ;

blockExpr
    : expressionInBlock EOS?
    | expressionInBlock EOS blockExpr
    ;

type_
    : typeName
    | literal
//    : typeName typeArgs?
//    | typeLit
    ;

typeName
    : qualifiedIdent
    | IDENTIFIER
    ;

typeParameters
    : L_BRACKET typeParameterDecl (COMMA typeParameterDecl)* R_BRACKET
    ;

typeParameterDecl
    : IDENTIFIER typeElement?
    ;

typeElement
    : typeTerm (OR typeTerm)*
    ;

typeTerm
    : UNDERLYING? type_
    ;
// Function declarations

functionDecl
    : FN IDENTIFIER typeParameters? signature block
    ;

// qualifiedIdent should only be used for types - a record select or an imported name
// (ie fmt.Print) are select expressions.
qualifiedIdent
    : IDENTIFIER DOT IDENTIFIER
    ;


signature // (a Int, b String) Int
    : parameters result?
    ;

result
//    : parameters // TODO return several types? or tuples?
    : COLON type_ // Int
    ;

// (a: Int, b, c: String)
parameters
    : L_PAREN (parameterDecl (COMMA parameterDecl)* COMMA?)? R_PAREN
    ;

parameterDecl
    : IDENTIFIER (COLON type_)?
//    : identifierList? ELLIPSIS? type_
    ;

// an expression already parenthesised or in a function block { }
// this is the expression entrypoint for functions
expressionInBlock
    : varDecl
    | fnLit
    | arithmeticExpr
    ;
//    | fnLit
//    | expression
//    ;

// restricted expr that does not include symbols we do not wish to be available
// when not parenthesised (such as on the RHS of an assignment or function literal)
//
// this is the expression entrypoint for assigment and var declarations
expression
    : fnLit
    | arithmeticExpr
    ;
//    : fnLit
//    | primaryExpr

arithmeticExpr
    : primaryExpr
    | unary_op = (PLUS | MINUS | EXCLAMATION | CARET | STAR | AMPERSAND | RECEIVE) arithmeticExpr
    | arithmeticExpr mul_op = (STAR | DIV | MOD | LSHIFT | RSHIFT | AMPERSAND | BIT_CLEAR) arithmeticExpr
    | arithmeticExpr add_op = (PLUS | MINUS | OR | CARET) arithmeticExpr
    | arithmeticExpr rel_op = (
        EQUALS
        | NOT_EQUALS
        | LESS
        | LESS_OR_EQUALS
        | GREATER
        | GREATER_OR_EQUALS
    ) arithmeticExpr
    | arithmeticExpr LOGICAL_AND arithmeticExpr
    | arithmeticExpr LOGICAL_OR arithmeticExpr
    | whenBlock
    | L_PAREN blockExpr R_PAREN
//    | fnLit
    ;

// primaryExpr is what can go on the left-hand-side of `.` or `()`
primaryExpr
    : operand
    | operandName fnCallArgs
    | primaryExpr (DOT IDENTIFIER | fnCallArgs) // select ecxpression (a.b)
    | listLiteral
    ;

listLiteral : L_BRACKET (expression (COMMA expression)* (COMMA)?)? R_BRACKET;

whenBlock
    : WHEN arithmeticExpr L_CURLY (whenCase EOS)+ R_CURLY
    ;

whenCase
    :   matchPattern ARROW arithmeticExpr
    ;

matchPattern
    : type_
    ;

operand
    : literal
    | operandName // typeArgs?
    ;

operandName
    : IDENTIFIER
    ;

fnCallArgs
    : L_PAREN (expression (COMMA expression)* COMMA?)? R_PAREN
    ;

literal
    : integer
    | string_
    | NIL_LIT
//    | fnLit
    ;

fnLit
    : FN (parameterDecl (COMMA parameterDecl)* COMMA?)? ARROW expression
    ;



integer
    : DECIMAL_LIT
    ;


string_
    : RAW_STRING_LIT
    | INTERPRETED_STRING_LIT
    ;

eos
    : SEMI
    | EOF
    | EOS
//  TODO  | {p.closingBracket()}?
    ;