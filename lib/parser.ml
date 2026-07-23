open Base
open Or_error.Let_syntax

module Precedence = struct
  type t = Lowest | Equals | LessGreater | Sum | Product | Prefix
  [@@deriving compare]

  let precedence_of_infix_operator = function
    | Token.Eq | NEq -> Equals
    | LT | GT -> LessGreater
    | Plus | Minus -> Sum
    | Asterisk | Slash -> Product
    | _ -> failwith "Not an infix operator"
end

let expect_semicolon = function
  | Token.Semicolon :: tl -> Ok tl
  | _ -> Or_error.error_string "Parsing Error: Missing required semicolon"

let rec infix_helper ~expr ~precedence = function
  | tok :: _ as tokens when Token.is_infix_operator tok ->
      parse_expression ~left_expr:(Some expr) ~precedence tokens
  | tokens -> Ok (expr, tokens)

and parse_expression ~left_expr ~precedence tokens =
  match left_expr with
  | None -> (
      match tokens with
      | Token.Int n :: tl -> infix_helper ~expr:(Ast.Integer n) ~precedence tl
      | Token.Ident var :: tl ->
          infix_helper ~expr:(Ast.Ident var) ~precedence tl
      | ((Token.Bang | Token.Minus) as tok) :: tokens ->
          let%bind expr, tl =
            parse_expression ~left_expr:None ~precedence:Precedence.Prefix
              tokens
          in
          infix_helper
            ~expr:(Ast.Prefix { operator = tok; expr })
            ~precedence tl
      | _ -> Or_error.error_string "Parsing Error: Invalid Expression")
  | Some left_expr -> (
      match tokens with
      | tok :: tl when Token.is_infix_operator tok ->
          let op_precedence = Precedence.precedence_of_infix_operator tok in
          if Precedence.compare op_precedence precedence > 0 then
            let%bind right_expr, tl' =
              parse_expression ~left_expr:None ~precedence:op_precedence tl
            in
            infix_helper
              ~expr:(Ast.Infix { left_expr; operator = tok; right_expr })
              ~precedence tl'
          else Ok (left_expr, tokens)
      | _ -> Or_error.error_string "Parsing Error: Invalid Expression")

let parse_let_statement = function
  | Token.Ident var :: Token.Assign :: tokens ->
      let%bind value, tl =
        parse_expression ~left_expr:None ~precedence:Precedence.Lowest tokens
      in
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
      let%bind value, tl =
        parse_expression ~left_expr:None ~precedence:Precedence.Lowest tokens
      in
      let%map tl' = expect_semicolon tl in
      (Ast.Return value, tl')
  | tokens ->
      let%map value, tl =
        parse_expression ~left_expr:None ~precedence:Precedence.Lowest tokens
      in
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

let%test_unit "parse infix expressions" =
  let inp =
    {|
      5+5;
      5-5;
      5*5;
      5/5;
      5>5;
      5<5;
      5==5;
      5!=5;
    |}
  in
  [%test_result: Ast.program Or_error.t] (parse_str inp)
    ~expect:
      (Ok
         [
           Ast.Expr
             (Infix
                {
                  left_expr = Ast.Integer 5;
                  operator = Token.Plus;
                  right_expr = Ast.Integer 5;
                });
           Ast.Expr
             (Infix
                {
                  left_expr = Ast.Integer 5;
                  operator = Token.Minus;
                  right_expr = Ast.Integer 5;
                });
           Ast.Expr
             (Infix
                {
                  left_expr = Ast.Integer 5;
                  operator = Token.Asterisk;
                  right_expr = Ast.Integer 5;
                });
           Ast.Expr
             (Infix
                {
                  left_expr = Ast.Integer 5;
                  operator = Token.Slash;
                  right_expr = Ast.Integer 5;
                });
           Ast.Expr
             (Infix
                {
                  left_expr = Ast.Integer 5;
                  operator = Token.GT;
                  right_expr = Ast.Integer 5;
                });
           Ast.Expr
             (Infix
                {
                  left_expr = Ast.Integer 5;
                  operator = Token.LT;
                  right_expr = Ast.Integer 5;
                });
           Ast.Expr
             (Infix
                {
                  left_expr = Ast.Integer 5;
                  operator = Token.Eq;
                  right_expr = Ast.Integer 5;
                });
           Ast.Expr
             (Infix
                {
                  left_expr = Ast.Integer 5;
                  operator = Token.NEq;
                  right_expr = Ast.Integer 5;
                });
         ])

let%test_unit "parse infix operator precedence" =
  let inp =
    {|
      -a*b;
      !-a;
      a+b-c;
      a*b/c;
      a+b/c;
      a+b*c+d/e-f;
      5>4==3<4;
      5<4!=3>4;
      3+4*5==3*1+4*5;
    |}
  in
  [%test_result: Ast.program Or_error.t] (parse_str inp)
    ~expect:
      (Ok
         [
           Ast.Expr
             (Infix
                {
                  left_expr =
                    Prefix { operator = Token.Minus; expr = Ast.Ident "a" };
                  operator = Token.Asterisk;
                  right_expr = Ast.Ident "b";
                });
           Ast.Expr
             (Prefix
                {
                  operator = Token.Bang;
                  expr = Prefix { operator = Token.Minus; expr = Ast.Ident "a" };
                });
           Ast.Expr
             (Infix
                {
                  left_expr =
                    Infix
                      {
                        left_expr = Ast.Ident "a";
                        operator = Token.Plus;
                        right_expr = Ast.Ident "b";
                      };
                  operator = Token.Minus;
                  right_expr = Ast.Ident "c";
                });
           Ast.Expr
             (Infix
                {
                  left_expr =
                    Infix
                      {
                        left_expr = Ast.Ident "a";
                        operator = Token.Asterisk;
                        right_expr = Ast.Ident "b";
                      };
                  operator = Token.Slash;
                  right_expr = Ast.Ident "c";
                });
           Ast.Expr
             (Infix
                {
                  left_expr = Ast.Ident "a";
                  operator = Token.Plus;
                  right_expr =
                    Infix
                      {
                        left_expr = Ast.Ident "b";
                        operator = Token.Slash;
                        right_expr = Ast.Ident "c";
                      };
                });
           Ast.Expr
             (Infix
                {
                  left_expr =
                    Infix
                      {
                        left_expr =
                          Infix
                            {
                              left_expr = Ast.Ident "a";
                              operator = Token.Plus;
                              right_expr =
                                Infix
                                  {
                                    left_expr = Ast.Ident "b";
                                    operator = Token.Asterisk;
                                    right_expr = Ast.Ident "c";
                                  };
                            };
                        operator = Token.Plus;
                        right_expr =
                          Infix
                            {
                              left_expr = Ast.Ident "d";
                              operator = Token.Slash;
                              right_expr = Ast.Ident "e";
                            };
                      };
                  operator = Token.Minus;
                  right_expr = Ast.Ident "f";
                });
           Ast.Expr
             (Infix
                {
                  left_expr =
                    Infix
                      {
                        left_expr = Ast.Integer 5;
                        operator = Token.GT;
                        right_expr = Ast.Integer 4;
                      };
                  operator = Token.Eq;
                  right_expr =
                    Infix
                      {
                        left_expr = Ast.Integer 3;
                        operator = Token.LT;
                        right_expr = Ast.Integer 4;
                      };
                });
           Ast.Expr
             (Infix
                {
                  left_expr =
                    Infix
                      {
                        left_expr = Ast.Integer 5;
                        operator = Token.LT;
                        right_expr = Ast.Integer 4;
                      };
                  operator = Token.NEq;
                  right_expr =
                    Infix
                      {
                        left_expr = Ast.Integer 3;
                        operator = Token.GT;
                        right_expr = Ast.Integer 4;
                      };
                });
           Ast.Expr
             (Infix
                {
                  left_expr =
                    Infix
                      {
                        left_expr = Ast.Integer 3;
                        operator = Token.Plus;
                        right_expr =
                          Infix
                            {
                              left_expr = Ast.Integer 4;
                              operator = Token.Asterisk;
                              right_expr = Ast.Integer 5;
                            };
                      };
                  operator = Token.Eq;
                  right_expr =
                    Infix
                      {
                        left_expr =
                          Infix
                            {
                              left_expr = Ast.Integer 3;
                              operator = Token.Asterisk;
                              right_expr = Ast.Integer 1;
                            };
                        operator = Token.Plus;
                        right_expr =
                          Infix
                            {
                              left_expr = Ast.Integer 4;
                              operator = Token.Asterisk;
                              right_expr = Ast.Integer 5;
                            };
                      };
                });
         ])
