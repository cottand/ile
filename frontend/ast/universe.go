package ast

const AnyTypeName = "any"
const NothingTypeName = "nothing"
const IntTypeName = "int"
const BoolTypeName = "bool"
const NumberTypeName = "number"
const UnitTypeName = "unit"
const TrueName = "true"
const FalseName = "false"

var IntType = &TypeTag{
	Name: IntTypeName,
}
var BoolType = &UnionType{
	Left:       FalseType,
	Right:      TrueType,
	Positioner: Range{},
}
var TrueType = &TypeTag{
	Name: TrueName,
}
var FalseType = &TypeTag{
	Name: FalseName,
}

var NilType = &TypeTag{
	Name: "Nil",
}
var UnitType = &TypeTag{
	Name: UnitTypeName,
}
