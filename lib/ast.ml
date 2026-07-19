open Base

type identifier = string [@@deriving compare, sexp]
type expression = Integer of int [@@deriving compare, sexp]

type letStatement = { name : identifier; value : expression }
[@@deriving compare, sexp]

type statement = Let of letStatement | Return of expression
[@@deriving compare, sexp]

type program = statement list [@@deriving compare, sexp]
