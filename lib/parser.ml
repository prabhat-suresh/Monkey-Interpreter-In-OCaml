open Base
open Or_error.Let_syntax

let expect_semicolon = function
  | Token.Semicolon :: tl -> Ok tl
  | _ -> Or_error.error_string "Parsing Error: Missing required semicolon"

let rec parse_expression = function
  | Token.Int n :: tl -> Ok (Ast.Integer n, tl)
  | Token.Ident var :: tl -> Ok (Ast.Ident var, tl)
  | ((Token.Bang | Token.Minus) as tok) :: tokens ->
      let%map expr, tl = parse_expression tokens in
      (Ast.Prefix { operator = tok; expr }, tl)
  | _ -> Or_error.error_string "Parsing Error: Invalid Expression"

let parse_let_statement = function
  | Token.Ident var :: Token.Assign :: tokens ->
      let%bind value, tl = parse_expression tokens in
      let%map tl' = expect_semicolon tl in
      ({ Ast.name = var; value }, tl')
  | _ ->
      Or_error.error_string
        "Parsing Error: Malformed let statement (expected 'ident = ...')"

let parse_statement = function
  | Token.Let :: tokens ->
      let%map let_stmt, tl = parse_let_statement tokens in
      (Ast.Let let_stmt, tl)
  | Token.Return :: tokens ->
      let%bind value, tl = parse_expression tokens in
      let%map tl' = expect_semicolon tl in
      (Ast.Return value, tl')
  | tokens ->
      let%map value, tl = parse_expression tokens in
      (* Optional semicolon for expression statements *)
      let tl' = match tl with Token.Semicolon :: tl' -> tl' | _ -> tl in
      (Ast.Expr value, tl')

let parse tokens =
  let rec helper acc = function
    (* helper for tail call optimization *)
    | [] -> Ok (List.rev acc)
    | tokens ->
        let%bind stmt, tl = parse_statement tokens in
        helper (stmt :: acc) tl
  in
  helper [] tokens

let parse_str inp = parse (Lexer.lex inp)

let%test_unit "parse let statements" =
  let inp =
    {|
      let x = 5;
      let y = 10;
      let foobar = 838383;
    |}
  in
  [%test_result: Ast.program Or_error.t] (parse_str inp)
    ~expect:
      (Ok
         [
           Ast.Let { name = "x"; value = Integer 5 };
           Ast.Let { name = "y"; value = Integer 10 };
           Ast.Let { name = "foobar"; value = Integer 838383 };
         ])

let%test_unit "parse return statements" =
  let inp = {|
      return 5;
      return 10;
      return 993322;
    |} in
  [%test_result: Ast.program Or_error.t] (parse_str inp)
    ~expect:
      (Ok
         [
           Ast.Return (Ast.Integer 5);
           Ast.Return (Ast.Integer 10);
           Ast.Return (Ast.Integer 993322);
         ])

let%test_unit "parse single expression statements" =
  let cases =
    [
      ("foobar;", [ Ast.Expr (Ident "foobar") ]);
      ("5;", [ Ast.Expr (Integer 5) ]);
    ]
  in
  List.iter cases ~f:(fun (inp, expected_ast) ->
      [%test_result: Ast.program Or_error.t] (parse_str inp)
        ~expect:(Ok expected_ast))

let%test_unit "parse prefix expressions" =
  let inp = {|
      !5;
      -15;
    |} in
  [%test_result: Ast.program Or_error.t] (parse_str inp)
    ~expect:
      (Ok
         [
           Ast.Expr (Prefix { operator = Token.Bang; expr = Ast.Integer 5 });
           Ast.Expr (Prefix { operator = Token.Minus; expr = Ast.Integer 15 });
         ])
