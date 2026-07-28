open Base

let eval_expression = function
  | Ast.Integer n -> Object.Integer (Int64.of_int n)
  | Ast.Boolean b -> if b then Object.True else Object.False
  | _ -> Object.Null

let eval_statement = function
  | Ast.Expr expr -> eval_expression expr
  | _ -> Object.Null

let eval (Ast.Program program) =
  List.fold program ~init:Object.Null ~f:(fun _ stmt -> eval_statement stmt)

let test_eval input = Lexer.lex input |> Parser.parse |> Or_error.map ~f:eval

let%test_unit "eval_integer_expression" =
  let inp = {|
        5
        10
    |} in
  let obj = test_eval inp in
  [%test_result: Object.t Or_error.t] obj
    ~expect:(Ok (Object.Integer (Int64.of_int 10)))

let%test_unit "eval_boolean_expression" =
  let inp = {|
        true
        false
    |} in
  let obj = test_eval inp in
  [%test_result: Object.t Or_error.t] obj ~expect:(Ok Object.False)
