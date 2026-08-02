open Base

type identifier = Ident of string [@@deriving compare, sexp]

type operator = Plus | Minus | Bang | Asterisk | Slash | LT | GT | Eq | NEq
[@@deriving compare, sexp]

let operator_of_token = function
  | Token.Plus -> Plus
  | Token.Minus -> Minus
  | Token.Bang -> Bang
  | Token.Asterisk -> Asterisk
  | Token.Slash -> Slash
  | Token.LT -> LT
  | Token.GT -> GT
  | Token.Eq -> Eq
  | Token.NEq -> NEq
  | _ -> failwith "Not a valid operator"

let string_of_operator = function
  | Plus -> "+"
  | Minus -> "-"
  | Bang -> "!"
  | Asterisk -> "*"
  | Slash -> "/"
  | LT -> "<"
  | GT -> ">"
  | Eq -> "=="
  | NEq -> "!="

type expression =
  | Integer of int
  | Boolean of bool
  | Identifier of string
  | Prefix of { operator : operator; expr : expression }
  | Infix of {
      left_expr : expression;
      operator : operator;
      right_expr : expression;
    }
  | IfElseExpression of {
      condition : expression;
      consequence : block_statement;
      alternative : block_statement option;
    }
  | Fn of function_expression
  | FnCall of { func : expression; arguments : expression list }

and statement =
  | Let of { name : identifier; value : expression }
  | Return of expression
  | Expr of expression

and block_statement = Block of statement list

and function_expression = {
  parameters : identifier list;
  body : block_statement;
}
[@@deriving compare, sexp]

type program = Program of statement list [@@deriving compare, sexp]
