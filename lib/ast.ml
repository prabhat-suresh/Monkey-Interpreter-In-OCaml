open Base

type identifier = string [@@deriving compare, sexp]

type expression =
  | Integer of int
  | Ident of string
  | Prefix of { operator : Token.t; expr : expression }
  | Infix of {
      left_expr : expression;
      operator : Token.t;
      right_expr : expression;
    }
[@@deriving compare, sexp]

type letStatement = { name : identifier; value : expression }
[@@deriving compare, sexp]

type statement =
  | Let of letStatement
  | Return of expression
  | Expr of expression
[@@deriving compare, sexp]

type program = statement list [@@deriving compare, sexp]
