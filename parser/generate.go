package parser

import "github.com/antlr4-go/antlr/v4"

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