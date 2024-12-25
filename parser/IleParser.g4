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
    : IDENTIFIER (type_ (ASSIGN expression)? | ASSIGN expression)
    ;

block
    : L_CURLY blockExpr? R_CURLY
    |
    ;

blockExpr
    : expression EOS?
    | expression EOS blockExpr
    ;

type_
    : typeName
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

qualifiedIdent
    : IDENTIFIER DOT IDENTIFIER
    ;
functionType
    : FN signature // fn (Int, String) Int
    ;

signature // (Int, String) Int
    : parameters result?
    ;

result
//    : parameters // TODO return several types? or tuples?
    : type_ // Int
    ;

parameters // (Int, String)
    : L_PAREN (parameterDecl (COMMA parameterDecl)* COMMA?)? R_PAREN
    ;

parameterDecl
    : IDENTIFIER type_
//    : identifierList? ELLIPSIS? type_
    ;

expression
    : primaryExpr
    | unary_op = (PLUS | MINUS | EXCLAMATION | CARET | STAR | AMPERSAND | RECEIVE) expression
    | expression mul_op = (STAR | DIV | MOD | LSHIFT | RSHIFT | AMPERSAND | BIT_CLEAR) expression
    | expression add_op = (PLUS | MINUS | OR | CARET) expression
    | expression rel_op = (
        EQUALS
        | NOT_EQUALS
        | LESS
        | LESS_OR_EQUALS
        | GREATER
        | GREATER_OR_EQUALS
    ) expression
    | expression LOGICAL_AND expression
    | expression LOGICAL_OR expression
    ;

primaryExpr
    : operand
    ;

operand
    : literal
    | L_PAREN expression R_PAREN
    ;

literal
    : integer
    | string_
    | functionLit
    ;

functionLit
    : FN typeParameters? signature block
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