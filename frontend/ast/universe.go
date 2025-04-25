package ast

const AnyTypeName = "any"
const NothingTypeName = "nothing"
const IntBuiltinTypeName = "int"
const BoolBuiltinTypeName = "bool"
const NumberBuiltinTypeName = "number"
const UnitTypeName = "unit"

var IntType = &TypeTag{
	Name: IntBuiltinTypeName,
}
var BoolType = &TypeTag{
	Name: BoolBuiltinTypeName,
}
var NilType = &TypeTag{
	Name: "Nil",
}
var UnitType = &TypeTag{
	Name: UnitTypeName,
}