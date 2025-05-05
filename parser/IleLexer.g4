lexer grammar IleLexer;

// Keywords

FN        : 'fn';
INTERFACE   : 'interface';
SELECT      : 'select';
CASE        : 'case';
STRUCT      : 'struct';
PACKAGE     : 'package';
CONST       : 'const';
FALLTHROUGH : 'fallthrough' -> mode(NLSEMI);
TYPE        : 'type';
IMPORT      : 'import';
WHEN        : 'when';
VAL         : 'val';


NIL_LIT: 'Nil' -> mode(NLSEMI);
IDENTIFIER: LETTER (LETTER | UNICODE_DIGIT)* -> mode(NLSEMI);

// Punctuation

L_PAREN        : '(';
R_PAREN        : ')' -> mode(NLSEMI);
L_CURLY        : '{';
R_CURLY        : '}' -> mode(NLSEMI);
L_BRACKET      : '[';
R_BRACKET      : ']' -> mode(NLSEMI);
ASSIGN         : '=';
COMMA          : ',';
SEMI           : ';';
COLON          : ':';
DOT            : '.';
PLUS_PLUS      : '++' -> mode(NLSEMI);
MINUS_MINUS    : '--' -> mode(NLSEMI);
ELLIPSIS       : '...';

// Logical

LOGICAL_OR  : '||';
LOGICAL_AND : '&&';

// Relation operators

EQUALS            : '==';
NOT_EQUALS        : '!=';
LESS              : '<';
LESS_OR_EQUALS    : '<=';
GREATER           : '>';
GREATER_OR_EQUALS : '>=';

// Arithmetic operators

OR         : '|';
DIV        : '/';
MOD        : '%';
LSHIFT     : '<<';
RSHIFT     : '>>';
BIT_CLEAR  : '&^';
UNDERLYING : '~';

// Unary operators

EXCLAMATION: '!';

// Mixed operators

PLUS      : '+';
MINUS     : '-';
CARET     : '^';
STAR      : '*';
AMPERSAND : '&';
RECEIVE   : '<-';

ARROW     : '->';

// Number literals

DECIMAL_LIT : ('0' | [1-9] ('_'? [0-9])*)   -> mode(NLSEMI);
BINARY_LIT  : '0' [bB] ('_'? BIN_DIGIT)+    -> mode(NLSEMI);
OCTAL_LIT   : '0' [oO]? ('_'? OCTAL_DIGIT)+ -> mode(NLSEMI);
HEX_LIT     : '0' [xX] ('_'? HEX_DIGIT)+    -> mode(NLSEMI);

FLOAT_LIT: (DECIMAL_FLOAT_LIT | HEX_FLOAT_LIT) -> mode(NLSEMI);

DECIMAL_FLOAT_LIT: DECIMALS ('.' DECIMALS? EXPONENT? | EXPONENT) | '.' DECIMALS EXPONENT?;

HEX_FLOAT_LIT: '0' [xX] HEX_MANTISSA HEX_EXPONENT;

fragment HEX_MANTISSA:
    ('_'? HEX_DIGIT)+ ('.' ( '_'? HEX_DIGIT)*)?
    | '.' HEX_DIGIT ('_'? HEX_DIGIT)*
;

fragment HEX_EXPONENT: [pP] [+-]? DECIMALS;

// Fragments

fragment ESCAPED_VALUE:
    '\\' (
        'u' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT
        | 'U' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT
        | [abfnrtv\\'"]
        | OCTAL_DIGIT OCTAL_DIGIT OCTAL_DIGIT
        | 'x' HEX_DIGIT HEX_DIGIT
    )
    | '\n'
;

// String literals

RAW_STRING_LIT         : '`' ~'`'* '`'                     -> mode(NLSEMI);
INTERPRETED_STRING_LIT : '"' (~["\\] | ESCAPED_VALUE)* '"' -> mode(NLSEMI);

// Hidden tokens

WS           : [ \t]+        -> channel(HIDDEN);
COMMENT      : '/*' .*? '*/' -> channel(HIDDEN);
TERMINATOR   : [\r\n]+       -> channel(HIDDEN);
//LINE_COMMENT : '//' ~[\r\n]* -> channel(HIDDEN);
LINE_COMMENT : '//' ~[\r\n]* -> channel(2);


fragment DECIMALS: [0-9] ('_'? [0-9])*;

fragment OCTAL_DIGIT: [0-7];

fragment HEX_DIGIT: [0-9a-fA-F];

fragment BIN_DIGIT: [01];

fragment EXPONENT: [eE] [+-]? DECIMALS;

fragment LETTER: UNICODE_LETTER | '_';

//[\p{Nd}] matches a digit zero through nine in any script except ideographic scripts
fragment UNICODE_DIGIT: [\p{Nd}];
//[\p{L}] matches any kind of letter from any language
fragment UNICODE_LETTER: [\p{L}];

mode NLSEMI;

// Treat whitespace as normal
WS_NLSEMI: [ \t]+ -> channel(HIDDEN);
// Ignore any comments that only span one line
// TODO COMMENT_NLSEMI      : '/*' ~[\r\n]*? '*/' -> channel(HIDDEN);
LINE_COMMENT_NLSEMI : '//' ~[\r\n]*       -> channel(2);
// Emit an EOS token for any newlines, semicolon, multiline comments or the EOF and
//return to normal lexing
EOS: ([\r\n]+ | ';' | '/*' .*? '*/' | EOF) -> mode(DEFAULT_MODE);

// Did not find an EOS, so go back to normal lexing
OTHER: -> mode(DEFAULT_MODE), channel(HIDDEN);