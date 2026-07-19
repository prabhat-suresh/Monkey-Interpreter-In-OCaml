open Base

let parse_expression = function
  | Token.Int n :: tl -> Ok (Ast.Integer n, tl)
  | _ -> Or_error.error_string "Parsing Error: Invalid Expression"

let parse_let_statement = function
  | Token.Ident var :: Token.Assign :: tokens -> (
      let open Or_error.Let_syntax in
      let%bind value, tl = parse_expression tokens in
      match tl with
      | Token.Semicolon :: tl' -> Ok ({ Ast.name = var; value }, tl')
      | _ ->
          Or_error.error_string
            "Parsing Error: Semicolon missing in Let Statement")
  | _ ->
      Or_error.error_string
        "Parsing Error: Identifier assignment missing in Let Statement"

let parse_statement = function
  | Token.Let :: tokens ->
      Or_error.map (parse_let_statement tokens) ~f:(fun (let_stmt, tl) ->
          (Ast.Let let_stmt, tl))
  | Token.Return :: tokens -> (
      let open Or_error.Let_syntax in
      let%bind value, tl = parse_expression tokens in
      match tl with
      | Token.Semicolon :: tl' -> Ok (Ast.Return value, tl')
      | _ ->
          Or_error.error_string
            "Parsing Error: Semicolon missing in Let Statement")
  | _ -> Or_error.error_string "Parsing Error: not a Statement"

let parse tokens =
  let rec helper acc = function
    (* helper for tail call optimization *)
    | [] -> Ok (List.rev acc)
    | _ as tokens ->
        let open Or_error.Let_syntax in
        let%bind stmt, tl = parse_statement tokens in
        helper (stmt :: acc) tl
  in
  helper [] tokens

let%test_unit "test_let_statements" =
  let inp =
    {|
      let x = 5;
      let y = 10;
      let foobar = 838383;
    |}
  in
  let program = parse (Lexer.lex inp) in
  [%test_eq: Ast.program Or_error.t] program
  @@ Ok
       [
         Ast.Let { name = "x"; value = Integer 5 };
         Ast.Let { name = "y"; value = Integer 10 };
         Ast.Let { name = "foobar"; value = Integer 838383 };
       ]

let%test_unit "test_return_statements" =
  let inp = {|
      return 5;
      return 10;
      return 993322;
    |} in
  let program = parse (Lexer.lex inp) in
  [%test_eq: Ast.program Or_error.t] program
  @@ Ok
       [
         Ast.Return (Ast.Integer 5);
         Ast.Return (Ast.Integer 10);
         Ast.Return (Ast.Integer 993322);
       ]
