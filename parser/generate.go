package parser

import (
	"github.com/antlr4-go/antlr/v4"
	"go/token"
)

// ANTLR_BIN is set by Nix during the build
//go:generate bash -c "$ANTLR_BIN -Dlanguage=Go -no-visitor -package parser *.g4"

// IleParserBase implementation.
type IleParserBase struct {
	*antlr.BaseParser
}



// Returns true if the current Token is a closing bracket (")" or "}")
func (p *IleParserBase) closingBracket() bool {
	stream := p.GetTokenStream()
	prevTokenType := stream.LA(1)
	return prevTokenType == IleParserR_PAREN || prevTokenType == IleParserR_CURLY;
}

var IleTokenInGo = map[int]token.Token {
	IleParserFN                     : token.FUNC,
	IleParserINTERFACE              : token.INTERFACE,
	IleParserSELECT                 : token.SELECT,
	IleParserCASE                   : token.CASE,
	IleParserSTRUCT                 : token.STRUCT,
	IleParserPACKAGE                : token.PACKAGE,
	//IleParserCONST                  :
	//IleParserFALLTHROUGH            = 8
	//IleParserTYPE                   = 9
	//IleParserIMPORT                 = 10
	//IleParserNIL_LIT                = 11
	//IleParserIDENTIFIER             = 12
	//IleParserL_PAREN                = 13
	//IleParserR_PAREN                = 14
	//IleParserL_CURLY                = 15
	//IleParserR_CURLY                = 16
	//IleParserL_BRACKET              = 17
	//IleParserR_BRACKET              = 18
	//IleParserASSIGN                 = 19
	//IleParserCOMMA                  = 20
	//IleParserSEMI                   = 21
	//IleParserCOLON                  = 22
	//IleParserDOT                    = 23
	//IleParserPLUS_PLUS              = 24
	//IleParserMINUS_MINUS            = 25
	//IleParserELLIPSIS               = 2
	IleParserLOGICAL_OR             : token.OR,
	IleParserLOGICAL_AND            : token.AND,
	IleParserEQUALS                 : token.EQL,
	IleParserNOT_EQUALS             : token.NEQ,
	IleParserLESS                   : token.LSS,
	IleParserLESS_OR_EQUALS         : token.LEQ,
	IleParserGREATER                : token.GTR,
	IleParserGREATER_OR_EQUALS      : token.GEQ,
	//IleParserOR                     = 35
	IleParserDIV                    : token.QUO,
	//IleParserMOD                    = 37
	//IleParserLSHIFT                 = 38
	//IleParserRSHIFT                 = 39
	//IleParserBIT_CLEAR              = 40
	//IleParserUNDERLYING             = 41
	IleParserEXCLAMATION            : token.NOT,
	IleParserPLUS                   : token.ADD,
	IleParserMINUS                  : token.SUB,
	//IleParserCARET                  = 45
	IleParserSTAR                   : token.MUL,
	//IleParserAMPERSAND              = 47
	//IleParserRECEIVE                = 48
	//IleParserDECIMAL_LIT            = 49
	//IleParserBINARY_LIT             = 50
	//IleParserOCTAL_LIT              = 51
	//IleParserHEX_LIT                = 52
	//IleParserFLOAT_LIT              = 53
	//IleParserDECIMAL_FLOAT_LIT      = 54
	//IleParserHEX_FLOAT_LIT          = 55
	//IleParserRAW_STRING_LIT         = 56
	//IleParserINTERPRETED_STRING_LIT = 57
	//IleParserWS                     = 58
	//IleParserCOMMENT                = 59
	//IleParserTERMINATOR             = 60
	//IleParserLINE_COMMENT           = 61
	//IleParserWS_NLSEMI              = 62
	//IleParserLINE_COMMENT_NLSEMI    = 63
	//IleParserEOS                    = 64
	//IleParserOTHER                  = 65
}