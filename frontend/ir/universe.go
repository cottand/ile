package ir

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
	IntType           = &TypeName{Name: IntTypeName}
	FloatType         = &TypeName{Name: FloatTypeName}
	StringType        = &TypeName{Name: StringTypeName}
	BoolTypeUnaliased = &UnionType{
		Left:       FalseType,
		Right:      TrueType,
		Positioner: Range{},
	}
	BoolType  = &TypeName{Name: BoolTypeName}
	TrueType  = &TypeName{Name: TrueName}
	FalseType = &TypeName{Name: FalseName}
	NilType   = &TypeName{Name: "Nil"}
	UnitType  = &TypeName{Name: UnitTypeName}
)
