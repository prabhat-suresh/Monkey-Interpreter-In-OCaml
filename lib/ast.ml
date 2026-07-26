open Base

type identifier = string [@@deriving compare, sexp]

type expression =
  | Integer of int
  | Boolean of bool
  | Ident of string
  | Prefix of { operator : Token.t; expr : expression }
  | Infix of {
      left_expr : expression;
      operator : Token.t;
      right_expr : expression;
    }
  | IfElseExpression of {
      condition : expression;
      consequence : block_statement;
      alternative : block_statement;
    }
[@@deriving compare, sexp]

and letStatement = { name : identifier; value : expression }
[@@deriving compare, sexp]

and statement =
  | Let of letStatement
  | Return of expression
  | Expr of expression
[@@deriving compare, sexp]

and block_statement = statement list [@@deriving compare, sexp]

type program = statement list [@@deriving compare, sexp]
