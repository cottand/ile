package ir

type CompileError struct {
	Message string
	At      Range
}
