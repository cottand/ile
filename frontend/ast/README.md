# AST Package

This package provides an Abstract Syntax Tree (AST) representation for Ile source code. Unlike the IR package, the AST is designed to be closer to the actual source code structure.

## Key Differences from IR

1. **No Type Information**: The AST doesn't include type information like `BaseTypes()` methods.
2. **No Synthetic Constructs**: The AST doesn't include synthetic constructs that can't be represented in source code (like `ir.FieldType` or `ir.GoType`).
3. **No Special Representations**: The AST doesn't have special representations for types like `Any` - these are just represented as type names.

## Structure

The AST is organised around these core interfaces:

- `Node`: Base interface for all AST nodes
- `Expr`: Interface for expression nodes
- `Stmt`: Interface for statement nodes
- `Type`: Interface for type nodes

All nodes implement the `Positioner` interface, which provides source code position information.

## Expression Types

- `Identifier`: Variable or function name
- `Literal`: Literal value (integer, string, etc.)
- `BinaryExpr`: Binary operation (a + b, etc.)
- `UnaryExpr`: Unary operation (!a, -b, etc.)
- `CallExpr`: Function call
- `FuncLit`: Function literal
- `SelectExpr`: Field selection (a.b)
- `ListLit`: List literal
- `StructLit`: Struct literal
- `WhenExpr`: Pattern matching
- `ParenExpr`: Parenthesized expression
- `VarDecl`: Variable declaration

## Statement Types

- `BlockStmt`: Block of statements
- `ExprStmt`: Expression used as a statement
- `FuncDecl`: Function declaration

## Type Representation

- `TypeName`: Named type
- `FuncType`: Function type
- `ListType`: List type
- `StructType`: Struct type
- `UnionType`: Union type (A | B)
- `IntersectionType`: Intersection type (A & B)
- `TypeApplication`: Type application (List<Int>)

## Position Tracking

All AST nodes include position information via the `Range` struct, which implements the `Positioner` interface.