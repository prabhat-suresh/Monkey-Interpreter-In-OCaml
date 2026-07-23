open Base

type t =
  | Illegal (* Eof token avoided for simplicity *)
  (* Identifiers and Literals *)
  | Ident of string
  | Int of int
  (* Operators *)
  | Assign
  | Plus
  | Minus
  | Bang
  | Asterisk
  | Slash
  | LT
  | GT
  | Eq
  | NEq
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
  | True
  | False
  | If
  | Else
  | Return
[@@deriving compare, sexp]

let is_infix_operator = function
  | Plus | Minus | Asterisk | Slash | GT | LT | Eq | NEq -> true
  | _ -> false
