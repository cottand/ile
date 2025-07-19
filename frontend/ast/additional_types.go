package ast

// BlockExpr represents a block of expressions.
// It implements both the Expr and Stmt interfaces to allow blocks to be used as both expressions and statements.
type BlockExpr struct {
	Range
	Expressions []Expr
}

func (e *BlockExpr) exprNode() {}
func (e *BlockExpr) stmtNode() {}

// TypeLiteral represents a type literal (e.g., a literal used as a type).
type TypeLiteral struct {
	Range
	Value *Literal
}

func (t *TypeLiteral) typeNode() {}
