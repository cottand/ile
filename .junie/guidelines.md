# Project Guidelines

- This repo is the implementation for a compiler for Ile, a programming language. Ile's type system is based on MLStruct
- MLStruct is its own separate type-system with a reference implementation in Scala, which can be found under the `./mlstruct`
symlink for reference.
- While Ile is based on MLStruct, it deviates in some parts for a couple reasons:
  - The implementation language is different and therfore algorithms and data structures are adapted differently
  - These are different languages with different syntaxes and design decisions. For example, Ile compiles to Go and
  therefore should be able to call Go libraries


## Project Structure

- `frontend/`: Contains the compiler frontend (type inference, IR generation)
  - `frontend/types/`: Type system implementation
  - `frontend/ir/`: Intermediate representation of Ile code
  - `frontend/ilerr/`: Error handling
- `parser/`: Contains the ANTLR-generated lexer and parser for Ile
- `backend/`: Contains the Go code generation
- `test/`: Contains end-to-end tests organized by feature
  - `test/expressions/`: Tests for expression evaluation
  - `test/functions/`: Tests for function definitions and calls
  - `test/interop/`: Tests for Go interoperability

## Type representations

There are two ways to represent types:
- ir.Type (for the IR)
- SimpleType (internal type representation that does not leave the `types` package).

We can go from ir.Type to SimpleType via TypeCtx.ProcessTypeDefs(), and the other way around via
TypeCtx.expandSimpleType().


## Language Features

- Ile is a functional language with Go interoperability
- Key features include:
  - Type inference (types don't need to be explicitly annotated)
  - Pattern matching using the `when` keyword
  - First-class functions
  - Ability to import and use Go packages


## Testing

- Tests are written as Ile source files with special comments
- Each test file should have a comment in the format: `//ile:compilerTest <expression> | <expected value>`
- The test framework compiles the Ile code to Go, executes it, and compares the result with the expected value
- To run an end-to-end test, use examples:
  - `go test -run TestInteropEndToEnd/boolFuncSimple3.ile`
  - `go test -run TestExpressionsEndToEnd/listDecl.ile`
- Not all tests will pass all the time, as the language is still WIP. Do not try to fix every failing test you see
  necessarily, unless it indicates regression.


## Contributing

- Think hard about the correctness of your code - invariants that hold in the Scala reference should hold in Ile.
- Stay true to the existing coding style when possible
  - eg, when it does not make a difference, I prefer appending to slices as opposed to writing to them via index
- Writing tests is desired, but as end-to-end as possible (eg, by adding the tested scenario as a source file in ile)
- When making changes to the type system, ensure compatibility with the MLStruct reference implementation
- When adding new language features, make sure to add corresponding tests
