open Base

let eval_bang_operator_expression = function
  | Object.True -> Object.False
  | Object.False | Object.Null -> Object.True
  | _ -> Object.False

let eval_minus_prefix_operator_expression = function
  | Object.Integer n -> Object.Integer Int64.(-n)
  | _ -> Object.Null

let eval_prefix_expression right = function
  | Ast.Bang -> eval_bang_operator_expression right
  | Ast.Minus -> eval_minus_prefix_operator_expression right
  | _ -> Null

let rec eval_expression = function
  | Ast.Integer n -> Object.Integer (Int64.of_int n)
  | Ast.Boolean b -> if b then Object.True else Object.False
  | Ast.Prefix { operator; expr } ->
      let right = eval_expression expr in
      eval_prefix_expression right operator
  | _ -> Object.Null

let eval_statement = function
  | Ast.Expr expr -> eval_expression expr
  | _ -> Object.Null

let eval (Ast.Program program) =
  List.fold program ~init:Object.Null ~f:(fun _ stmt -> eval_statement stmt)

let test_eval input = Lexer.lex input |> Parser.parse |> Or_error.map ~f:eval

let%test_unit "eval integer expression" =
  let cases = [ ("5", 5); ("10", 10); ("-5", -5); ("-10", -10) ] in
  List.iter cases ~f:(fun (inp, expected) ->
      let obj = test_eval inp in
      [%test_result: Object.t Or_error.t] obj
        ~expect:(Ok (Object.Integer (Int64.of_int expected))))

let%test_unit "eval boolean expression" =
  let inp = {|
        true
        false
    |} in
  let obj = test_eval inp in
  [%test_result: Object.t Or_error.t] obj ~expect:(Ok Object.False)

let%test_unit "bang operator" =
  let cases =
    [
      ("!true", Object.False);
      ("!false", True);
      ("!5", False);
      ("!!true", True);
      ("!!false", False);
      ("!!5", True);
    ]
  in
  List.iter cases ~f:(fun (inp, expected) ->
      let obj = test_eval inp in
      [%test_result: Object.t Or_error.t] obj ~expect:(Ok expected))
