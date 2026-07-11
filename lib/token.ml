open Base

type t =
  | Illegal (* Eof token avoided for simplicity *)
  (* Identifiers and Literals *)
  | Ident of string
  | Int of int
  (* Operators *)
  | Assign
  | Plus
  (* Delimiters *)
  | Comma
  | Semicolon
  | LParen
  | RParen
  | LBrace
  | RBrace
  (* Keywords *)
  | Function
  | Let
[@@deriving compare, sexp]
