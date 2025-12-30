# Grammar for owl-lang

```
# Owl Language — Complete BNF-like Grammar (v0.3)
# - Functions have no `fn` keyword: Identifier "(" ... ")" "->" Type Block
# - Unified declaration/assignment: `name = expr` (type annotation optional on LHS)
# - Arrays are dynamic: `[Type]` annotations; literals like `["Alice", "Bob"]` or `[]`
# - Tuples: `tuple User(name string, email string, password string)`
# - Tuple construction supports positional and named-field forms:
#       User("Alice", "alice@x", "secret")
#       User(name = "Alice", password = "secret", email = "alice@x")
# - Primitive types: string, int(32), float(32), bool(1), byte(8)

############################
# Lexical structure
############################

Program               ::= { TopLevelDecl }

TopLevelDecl          ::= Stmt
                        | FuncDecl
                        | TupleDecl

Identifier            ::= Letter { Letter | Digit | "_" }
Letter                ::= "A".."Z" | "a".."z"
Digit                 ::= "0".."9"

WS                    ::= (" " | "\t" | "\r" | "\n")+
LineComment           ::= "//" { any-char-except-newline }
BlockComment          ::= "/*" { any-char } "*/"

############################
# Types
############################

Type                  ::= PrimitiveType
                        | ArrayType
                        | TupleType

PrimitiveType         ::= "string" | "int" | "float" | "bool" | "byte"
ArrayType             ::= "[" Type "]"
TupleType             ::= Identifier

# Literals

StringLiteral         ::= "\"" { StringChar } "\""
StringChar            ::= any-char-except-quote | EscapeSeq
EscapeSeq             ::= "\\" ( "n" | "t" | "\\" | "\"" )
# Note: Strings are multiline. Newline characters are allowed inside string literals.
# The lexer must not emit NEWLINE tokens inside a string; all content between quotes belongs to the string token.

IntLiteral            ::= Digit { Digit }
FloatLiteral          ::= Digit { Digit } "." Digit { Digit }
BoolLiteral           ::= "true" | "false"

Literal               ::= StringLiteral
                        | FloatLiteral
                        | IntLiteral
                        | BoolLiteral

############################
# Statements and blocks
############################

Stmt                  ::= VarDeclOrAssign ";"
                        | IfStmt
                        | LoopStmt
                        | Block
                        | ReturnStmt ";"

Block                 ::= "{" { Stmt } "}"

VarDeclOrAssign       ::= LHS "=" Expr

LHS                   ::= Identifier [ TypeAnnotation ]

TypeAnnotation        ::= PrimitiveType
                        | TupleType
                        | ArrayType
                        | ArrayTypeSuffix

ArrayTypeSuffix       ::= "[" PrimitiveType "]"

ReturnStmt            ::= "return" [ Expr ]

############################
# Control flow
############################

IfStmt                ::= "if" Expr Block [ "else" Block ]

LoopStmt              ::= "loop" Expr Block [ "else" Block ]

############################
# Functions (no `fn` keyword)
############################

FuncDecl              ::= Identifier "(" ParamList? ")" ReturnArrow? Block

ParamList             ::= Param { "," Param }
Param                  ::= Identifier TypeRequired
TypeRequired           ::= PrimitiveType | TupleType | ArrayType

ReturnArrow           ::= "->" Type

############################
# Tuples
############################

TupleDecl             ::= "tuple" Identifier "(" TupleFieldList ")"

TupleFieldList        ::= TupleField { "," TupleField }
TupleField            ::= Identifier PrimitiveType
                        | Identifier TupleType
                        | Identifier ArrayType

# Tuple construction (positional or named)
TupleConstruct        ::= Identifier "(" ( ArgList | NamedArgList )? ")"

# Positional arguments
ArgList               ::= Expr { "," Expr }

# Named arguments: any order, each field is `Identifier = Expr`
# - No duplicate field names allowed (semantic check).
# - All fields not provided use defaults if supported (currently none);
#   otherwise, all declared fields must be provided (semantic check).
NamedArgList          ::= FieldAssign { "," FieldAssign }
FieldAssign           ::= Identifier "=" Expr

############################
# Expressions
############################

Expr                  ::= OrExpr

OrExpr                ::= AndExpr { "||" AndExpr }
AndExpr               ::= EqualityExpr { "&&" EqualityExpr }

EqualityExpr          ::= RelExpr { ( "==" | "!=" ) RelExpr }
RelExpr               ::= AddExpr { ( "<" | "<=" | ">" | ">=" ) AddExpr }
AddExpr               ::= MulExpr { ( "+" | "-" ) MulExpr }
MulExpr               ::= UnaryExpr { ( "*" | "/" | "%" ) UnaryExpr }

UnaryExpr             ::= ( "+" | "-" | "!" ) UnaryExpr
                        | PrimaryExpr

PrimaryExpr           ::= Literal
                        | Identifier
                        | TupleConstruct
                        | ArrayLiteral
                        | "(" Expr ")"
                        | IndexExpr
                        | CallExpr

ArrayLiteral          ::= "[" ExprList? "]"
ExprList              ::= Expr { "," Expr }

IndexExpr             ::= PrimaryExpr "[" Expr "]"

CallExpr              ::= PrimaryExpr "(" ArgList? ")"

# Precedence summary:
#   Call/Index/TupleConstruct
#   Unary (+ - !)
#   * / %
#   + -
#   < <= > >=
#   == !=
#   &&
#   ||

############################
# Disambiguation notes
############################
# - Top-level `Identifier "("` followed by `")"` or args parses as FuncDecl if immediately followed by `ReturnArrow? Block`.
# - Otherwise, inside expressions, `Identifier "("` parses as CallExpr or TupleConstruct
#   depending on identifier binding (function vs tuple type).
# - `Identifier "=" Expr` at statement level is VarDeclOrAssign.
# - Unified decl/assign resolved by scope:
#     - Not in scope => declaration
#     - In scope => assignment
# - `ArrayTypeSuffix` allows `name [int] = []`.

############################
# Semantics overview (non-normative)
############################
# - Tuple construction:
#     - Positional: fields map by declared order.
#     - Named: fields map by name, order-independent.
#     - Mixed positional + named is disallowed (keep grammar simple). If desired later, define mix rules.
#     - Duplicate field names in named form are errors.
#     - Missing fields are errors unless default values are introduced in a future revision.
# - Types: string (heap-managed), int32, float32, bool(1 byte), byte(8-bit).
# - Arrays: dynamic size; `[]` requires a type on LHS or contextual inference.
# - Functions: parameters readonly; `-> Type` optional for procedures (maps to `void`).
# - Loop `else` runs only if body did not execute.

```
