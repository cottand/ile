package frontend

import (
	"github.com/antlr4-go/antlr/v4"
	"github.com/cottand/ile/ir"
	"github.com/cottand/ile/parser"
)

type listener struct {
	*parser.BaseIleParserListener

	file   ir.File
	errors []ir.CompileError
}
func (l *listener) Result() (ir.File, []ir.CompileError) {
	return l.file, l.errors
}

func (l *listener) EnterPackageClause(ctx *parser.PackageClauseContext) {
	l.file.PkgName = ctx.IDENTIFIER().GetText()
}

func (l *listener) EnterDeclaration(ctx *parser.DeclarationContext) {
}

func (l *listener) EnterVarDecl(ctx *parser.VarDeclContext)  {
	// declaration is at file source
	if _, ok := ctx.GetParent().GetParent().(*parser.SourceFileContext); ok {
		l.file.Declarations = append(l.file.Declarations, ir.ValDecl{
			Name: ctx.IDENTIFIER().GetText(),
		})
	}
}

func (l *listener) VisitErrorNode(node antlr.ErrorNode) {
	l.errors = append(l.errors, ir.CompileError{
		Message: "error at: " + node.GetText(),
	})
}
func (l *listener) VisitTerminal(node antlr.TerminalNode) {
	l.errors = append(l.errors, ir.CompileError{
		Message: "terminal at: " + node.GetText(),
	})
}
