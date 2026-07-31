open Base

let eval_bang_operator_expression obj =
  Object.native_bool_to_boolean_object @@ not @@ Object.is_truthy obj

let eval_minus_prefix_operator_expression = function
  | Object.Integer n -> Object.Integer Int64.(-n)
  | obj -> Err (Printf.sprintf "unknown operator: -%s" (Object.type_of obj))

let eval_prefix_expression right = function
  | Ast.Bang -> eval_bang_operator_expression right
  | Ast.Minus -> eval_minus_prefix_operator_expression right
  | op ->
      Err
        (Printf.sprintf "unknown operator: %s %s"
           (Ast.string_of_operator op)
           (Object.type_of right))

let eval_integer_infix_expression left right = function
  | Ast.Plus -> Object.Integer Int64.(left + right)
  | Ast.Minus -> Object.Integer Int64.(left - right)
  | Ast.Asterisk -> Object.Integer Int64.(left * right)
  | Ast.Slash -> Object.Integer Int64.(left / right)
  | Ast.LT -> Object.native_bool_to_boolean_object Int64.(left < right)
  | Ast.GT -> Object.native_bool_to_boolean_object Int64.(left > right)
  | Ast.Eq -> Object.native_bool_to_boolean_object Int64.(left = right)
  | Ast.NEq -> Object.native_bool_to_boolean_object Int64.(left <> right)
  | op ->
      Err
        (Printf.sprintf "unknown operator: INTEGER %s INTEGER"
           (Ast.string_of_operator op))

let eval_infix_expression ((left, op, right) as tuple) =
  if String.(Object.type_of left = Object.type_of right) then
    match tuple with
    | Object.Integer left, op, Object.Integer right ->
        eval_integer_infix_expression left right op
    | _, Ast.Eq, _ ->
        Object.native_bool_to_boolean_object (Object.compare left right = 0)
    | _, Ast.NEq, _ ->
        Object.native_bool_to_boolean_object (Object.compare left right <> 0)
    | _ ->
        Err
          (Printf.sprintf "unknown operator: %s %s %s" (Object.type_of left)
             (Ast.string_of_operator op)
             (Object.type_of right))
  else
    Err
      (Printf.sprintf "type mismatch: %s %s %s" (Object.type_of left)
         (Ast.string_of_operator op)
         (Object.type_of right))

let rec eval_if_else_expression condition (Ast.Block consequence) alternative =
  match eval_expression condition with
  | Object.Err _ as err -> err
  | condition -> (
      if Object.is_truthy condition then
        eval_statement_list consequence ~pass_return:true
      else
        match alternative with
        | None -> Object.Null
        | Some (Ast.Block alternative) ->
            eval_statement_list alternative ~pass_return:true)

and eval_expression = function
  | Ast.Integer n -> Object.Integer (Int64.of_int n)
  | Ast.Boolean b -> Object.native_bool_to_boolean_object b
  | Ast.Prefix { operator; expr } -> (
      match eval_expression expr with
      | Err _ as right -> right
      | right -> eval_prefix_expression right operator)
  | Ast.Infix { left_expr; operator; right_expr } -> (
      match (eval_expression left_expr, eval_expression right_expr) with
      | (Err _ as left), _ -> left
      | _, (Err _ as right) -> right
      | left, right -> eval_infix_expression (left, operator, right))
  | Ast.IfElseExpression { condition; consequence; alternative } ->
      eval_if_else_expression condition consequence alternative
  | _ -> Object.Null

and eval_statement = function
  | Ast.Expr expr -> eval_expression expr
  | Return expr -> (
      match eval_expression expr with Err _ as err -> err | ret -> Return ret)
  | _ -> Object.Null

and eval_statement_list statements ~pass_return =
  List.fold_until statements ~init:Object.Null
    ~f:(fun _ stmt ->
      match eval_statement stmt with
      | Return obj as ret -> Stop (if pass_return then ret else obj)
      | Err _ as err -> Stop err
      | obj -> Continue obj)
    ~finish:Fn.id

let eval (Ast.Program program) = eval_statement_list program ~pass_return:false
let test_eval input = Lexer.lex input |> Parser.parse |> Or_error.map ~f:eval

let test_helper cases ~expected_to_result =
  List.iter cases ~f:(fun (inp, expected) ->
      let obj = test_eval inp in
      [%test_result: Object.t Or_error.t] obj
        ~expect:(expected_to_result expected))

let%test_unit "eval integer expression" =
  let cases =
    [
      ("5", 5);
      ("10", 10);
      ("-5", -5);
      ("-10", -10);
      ("5 + 5 + 5 + 5 - 10", 10);
      ("2 * 2 * 2 * 2 * 2", 32);
      ("-50 + 100 + -50", 0);
      ("5 * 2 + 10", 20);
      ("5 + 2 * 10", 25);
      ("20 + 2 * -10", 0);
      ("50 / 2 * 2 + 10", 60);
      ("2 * (5 + 10)", 30);
      ("3 * 3 * 3 + 10", 37);
      ("3 * (3 * 3) + 10", 37);
      ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50);
    ]
  in
  test_helper cases ~expected_to_result:(fun expected ->
      Ok (Object.Integer (Int64.of_int expected)))

let%test_unit "eval boolean expression" =
  let cases =
    [
      ("true", Object.True);
      ("false", False);
      ("1 < 2", True);
      ("1 > 2", False);
      ("1 < 1", False);
      ("1 > 1", False);
      ("1 == 1", True);
      ("1 != 1", False);
      ("1 == 2", False);
      ("1 != 2", True);
      ("true == true", True);
      ("false == false", True);
      ("true == false", False);
      ("true != false", True);
      ("false != true", True);
      ("(1 < 2) == true", True);
      ("(1 < 2) == false", False);
      ("(1 > 2) == true", False);
      ("(1 > 2) == false", True);
    ]
  in
  test_helper cases ~expected_to_result:(fun expected -> Ok expected)

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
  test_helper cases ~expected_to_result:(fun expected -> Ok expected)

let%test_unit "If Else Expressions" =
  let ten, twenty =
    (Object.Integer (Int64.of_int 10), Object.Integer (Int64.of_int 20))
  in
  let cases =
    [
      ("if (true) { 10 }", ten);
      ("if (false) { 10 }", Object.Null);
      ("if (1) { 10 }", ten);
      ("if (1 < 2) { 10 }", ten);
      ("if (1 > 2) { 10 }", Object.Null);
      ("if (1 > 2) { 10 } else { 20 }", twenty);
      ("if (1 < 2) { 10 } else { 20 }", ten);
    ]
  in
  test_helper cases ~expected_to_result:(fun expected -> Ok expected)

let%test_unit "Return Statements" =
  let cases =
    [
      ("return 10;", 10);
      ("return 10; 9;", 10);
      ("return 2 * 5; 9;", 10);
      ("9; return 2 * 5; 9;", 10);
      ( {|if (10 > 1) {
          if (10 > 1) {
            return 10;
          }
          return 1;
        }
        |},
        10 );
    ]
  in
  test_helper cases ~expected_to_result:(fun expected ->
      Ok (Object.Integer (Int64.of_int expected)))

let%test_unit "Error Handling" =
  let cases =
    [
      ("5 + true;", "type mismatch: INTEGER + BOOLEAN");
      ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN");
      ("-true", "unknown operator: -BOOLEAN");
      ("true + false;", "unknown operator: BOOLEAN + BOOLEAN");
      ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN");
      ("if (10 > 1) { true + false; }", "unknown operator: BOOLEAN + BOOLEAN");
      ( {|if (10 > 1) {
      if (10 > 1) {
        return true + false;
      }
    return 1;
  }|},
        "unknown operator: BOOLEAN + BOOLEAN" );
    ]
  in
  test_helper cases ~expected_to_result:(fun expected ->
      Ok (Object.Err expected))
