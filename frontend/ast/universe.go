package ast

const (
	AnyTypeName     = "Any"
	NothingTypeName = "Nothing"
	IntTypeName     = "Int"
	FloatTypeName   = "Float"
	StringTypeName  = "String"
	BoolTypeName    = "Bool"
	UnitTypeName    = "Unit"
	TrueName        = "True"
	FalseName       = "False"
	ArrayTypeName   = "Array"
)

var (
	IntType    = &TypeName{Name: IntTypeName}
	StringType = &TypeName{Name: StringTypeName}
	BoolType   = &UnionType{
		Left:       FalseType,
		Right:      TrueType,
		Positioner: Range{},
	}
	TrueType  = &TypeName{Name: TrueName}
	FalseType = &TypeName{Name: FalseName}
	NilType   = &TypeName{Name: "Nil"}
	UnitType  = &TypeName{Name: UnitTypeName}
)
