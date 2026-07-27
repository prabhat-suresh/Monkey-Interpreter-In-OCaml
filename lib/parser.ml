open Base
open Or_error.Let_syntax

module Precedence = struct
  type t = Lowest | Equals | LessGreater | Sum | Product | Prefix
  [@@deriving compare]

  (* To be called only on infix operators *)
  let precedence_of_infix_operator = function
    | Ast.Eq | NEq -> Equals
    | LT | GT -> LessGreater
    | Plus | Minus -> Sum
    | Asterisk | Slash -> Product
    | _ -> failwith "Not an infix operator"
end

let expect token = function
  | tok :: tl when Token.compare tok token = 0 -> Ok tl
  | _ ->
      Or_error.error_string
      @@ Printf.sprintf "Parsing Error: Missing required token: %s"
      @@ Sexp.to_string_hum @@ Token.sexp_of_t token

let maybe_consume token = function
  | tok :: tl when Token.compare tok token = 0 -> tl
  | tokens -> tokens

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
          infix_helper ~expr:(Ast.Identifier var) ~precedence tl
      | Token.True :: tl -> infix_helper ~expr:(Ast.Boolean true) ~precedence tl
      | Token.False :: tl ->
          infix_helper ~expr:(Ast.Boolean false) ~precedence tl
      | ((Token.Bang | Token.Minus) as tok) :: tl ->
          let%bind expr, tl' =
            parse_expression ~left_expr:None ~precedence:Precedence.Prefix tl
          in
          infix_helper
            ~expr:(Ast.Prefix { operator = Ast.operator_of_token tok; expr })
            ~precedence tl'
      | Token.LParen :: tl ->
          let%bind expr, tl' =
            parse_expression ~left_expr:None ~precedence:Precedence.Lowest tl
          in
          let%bind tl'' = expect Token.RParen tl' in
          infix_helper ~expr ~precedence tl''
      | Token.If :: tl -> parse_if_else_expression tl
      | Token.Function :: tl ->
          let%bind func, tl' = parse_function_literals tl in
          infix_helper ~expr:func ~precedence tl'
      | _ -> Or_error.error_string "Parsing Error: Invalid Expression")
  | Some left_expr -> (
      match tokens with
      | Token.LParen :: tl ->
          let%bind arguments, tl' = parse_arguments tl in
          infix_helper
            ~expr:(Ast.FnCall { func = left_expr; arguments })
            ~precedence tl'
      | tok :: tl when Token.is_infix_operator tok ->
          let op_precedence =
            Precedence.precedence_of_infix_operator (Ast.operator_of_token tok)
          in
          if Precedence.compare op_precedence precedence > 0 then
            let%bind right_expr, tl' =
              parse_expression ~left_expr:None ~precedence:op_precedence tl
            in
            infix_helper
              ~expr:
                (Ast.Infix
                   {
                     left_expr;
                     operator = Ast.operator_of_token tok;
                     right_expr;
                   })
              ~precedence tl'
          else Ok (left_expr, tokens)
      | _ -> Or_error.error_string "Parsing Error: Invalid Expression")

and parse_let_statement = function
  | Token.Ident var :: Token.Assign :: tokens ->
      let%bind value, tl =
        parse_expression ~left_expr:None ~precedence:Precedence.Lowest tokens
      in
      let%map tl' = expect Token.Semicolon tl in
      (Ast.Let { name = Ast.Ident var; value }, tl')
  | _ ->
      Or_error.error_string
        "Parsing Error: Malformed let statement (expected 'ident = ...')"

and parse_statement = function
  | Token.Let :: tokens -> parse_let_statement tokens
  | Token.Return :: tokens ->
      let%bind value, tl =
        parse_expression ~left_expr:None ~precedence:Precedence.Lowest tokens
      in
      let%map tl' = expect Token.Semicolon tl in
      (Ast.Return value, tl')
  | tokens ->
      let%map value, tl =
        parse_expression ~left_expr:None ~precedence:Precedence.Lowest tokens
      in
      (* Optional semicolon for expression statements *)
      let tl' = maybe_consume Token.Semicolon tl in
      (Ast.Expr value, tl')

and parse_block_statement = function
  | Token.LBrace :: tokens ->
      let rec helper block = function
        | Token.RBrace :: tl -> Ok (Ast.Block (List.rev block), tl)
        | tl ->
            let%bind stmt, tl' = parse_statement tl in
            helper (stmt :: block) tl'
      in
      helper [] tokens
  | _ ->
      Or_error.error_string "Expected LBrace in If statement, but it's missing"

and parse_if_else_expression tokens =
  let%bind tokens = expect Token.LParen tokens in
  let%bind condition, tokens =
    parse_expression ~left_expr:None ~precedence:Lowest tokens
  in
  let%bind tokens = expect Token.RParen tokens in
  let%bind consequence, tokens = parse_block_statement tokens in
  match tokens with
  | Token.Else :: tokens ->
      let%map alternative, tokens = parse_block_statement tokens in
      ( Ast.IfElseExpression
          { condition; consequence; alternative = Some alternative },
        tokens )
  | _ ->
      Ok
        ( Ast.IfElseExpression { condition; consequence; alternative = None },
          tokens )

and parse_parameters tokens =
  let%bind tl = expect Token.LParen tokens in
  let rec helper params = function
    | Token.RParen :: tl' -> Ok (List.rev params, tl')
    | Token.Ident param :: tl' ->
        let tl'' = maybe_consume Token.Comma tl' in
        helper (Ast.Ident param :: params) tl''
    | _ ->
        Or_error.error_string
          "Parsing Error: ill-formed parameters in function definition"
  in
  helper [] tl

and parse_function_literals tokens =
  let%bind parameters, tl = parse_parameters tokens in
  let%map body, tl' = parse_block_statement tl in
  (Ast.Fn { parameters; body }, tl')

and parse_arguments tokens =
  let rec helper args = function
    | Token.RParen :: tl' -> Ok (List.rev args, tl')
    | tokens ->
        let%bind expr, tl =
          parse_expression ~left_expr:None ~precedence:Lowest tokens
        in
        let tl' = maybe_consume Token.Comma tl in
        helper (expr :: args) tl'
  in
  helper [] tokens

let parse tokens =
  let rec helper acc = function
    (* helper for tail call optimization *)
    | [] -> Ok (Ast.Program (List.rev acc))
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
         (Ast.Program
            [
              Ast.Let { name = Ast.Ident "x"; value = Ast.Integer 5 };
              Ast.Let { name = Ast.Ident "y"; value = Ast.Integer 10 };
              Ast.Let { name = Ast.Ident "foobar"; value = Ast.Integer 838383 };
            ]))

let%test_unit "parse return statements" =
  let inp = {|
      return 5;
      return 10;
      return 993322;
    |} in
  [%test_result: Ast.program Or_error.t] (parse_str inp)
    ~expect:
      (Ok
         (Ast.Program
            [
              Ast.Return (Ast.Integer 5);
              Ast.Return (Ast.Integer 10);
              Ast.Return (Ast.Integer 993322);
            ]))

let%test_unit "parse single expression statements" =
  let cases =
    [
      ("foobar;", [ Ast.Expr (Ast.Identifier "foobar") ]);
      ("5;", [ Ast.Expr (Ast.Integer 5) ]);
    ]
  in
  List.iter cases ~f:(fun (inp, expected_ast) ->
      [%test_result: Ast.program Or_error.t] (parse_str inp)
        ~expect:(Ok (Ast.Program expected_ast)))

let%test_unit "parse prefix expressions" =
  let inp = {|
      !5;
      -15;
    |} in
  [%test_result: Ast.program Or_error.t] (parse_str inp)
    ~expect:
      (Ok
         (Ast.Program
            [
              Ast.Expr
                (Ast.Prefix { operator = Ast.Bang; expr = Ast.Integer 5 });
              Ast.Expr
                (Ast.Prefix { operator = Ast.Minus; expr = Ast.Integer 15 });
            ]))

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
         (Ast.Program
            [
              Ast.Expr
                (Ast.Infix
                   {
                     left_expr = Ast.Integer 5;
                     operator = Ast.Plus;
                     right_expr = Ast.Integer 5;
                   });
              Ast.Expr
                (Ast.Infix
                   {
                     left_expr = Ast.Integer 5;
                     operator = Ast.Minus;
                     right_expr = Ast.Integer 5;
                   });
              Ast.Expr
                (Ast.Infix
                   {
                     left_expr = Ast.Integer 5;
                     operator = Ast.Asterisk;
                     right_expr = Ast.Integer 5;
                   });
              Ast.Expr
                (Ast.Infix
                   {
                     left_expr = Ast.Integer 5;
                     operator = Ast.Slash;
                     right_expr = Ast.Integer 5;
                   });
              Ast.Expr
                (Ast.Infix
                   {
                     left_expr = Ast.Integer 5;
                     operator = Ast.GT;
                     right_expr = Ast.Integer 5;
                   });
              Ast.Expr
                (Ast.Infix
                   {
                     left_expr = Ast.Integer 5;
                     operator = Ast.LT;
                     right_expr = Ast.Integer 5;
                   });
              Ast.Expr
                (Ast.Infix
                   {
                     left_expr = Ast.Integer 5;
                     operator = Ast.Eq;
                     right_expr = Ast.Integer 5;
                   });
              Ast.Expr
                (Ast.Infix
                   {
                     left_expr = Ast.Integer 5;
                     operator = Ast.NEq;
                     right_expr = Ast.Integer 5;
                   });
            ]))

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
         (Ast.Program
            [
              Ast.Expr
                (Ast.Infix
                   {
                     left_expr =
                       Ast.Prefix
                         { operator = Ast.Minus; expr = Ast.Identifier "a" };
                     operator = Ast.Asterisk;
                     right_expr = Ast.Identifier "b";
                   });
              Ast.Expr
                (Ast.Prefix
                   {
                     operator = Ast.Bang;
                     expr =
                       Ast.Prefix
                         { operator = Ast.Minus; expr = Ast.Identifier "a" };
                   });
              Ast.Expr
                (Ast.Infix
                   {
                     left_expr =
                       Ast.Infix
                         {
                           left_expr = Ast.Identifier "a";
                           operator = Ast.Plus;
                           right_expr = Ast.Identifier "b";
                         };
                     operator = Ast.Minus;
                     right_expr = Ast.Identifier "c";
                   });
              Ast.Expr
                (Ast.Infix
                   {
                     left_expr =
                       Ast.Infix
                         {
                           left_expr = Ast.Identifier "a";
                           operator = Ast.Asterisk;
                           right_expr = Ast.Identifier "b";
                         };
                     operator = Ast.Slash;
                     right_expr = Ast.Identifier "c";
                   });
              Ast.Expr
                (Ast.Infix
                   {
                     left_expr = Ast.Identifier "a";
                     operator = Ast.Plus;
                     right_expr =
                       Ast.Infix
                         {
                           left_expr = Ast.Identifier "b";
                           operator = Ast.Slash;
                           right_expr = Ast.Identifier "c";
                         };
                   });
              Ast.Expr
                (Ast.Infix
                   {
                     left_expr =
                       Ast.Infix
                         {
                           left_expr =
                             Ast.Infix
                               {
                                 left_expr = Ast.Identifier "a";
                                 operator = Ast.Plus;
                                 right_expr =
                                   Ast.Infix
                                     {
                                       left_expr = Ast.Identifier "b";
                                       operator = Ast.Asterisk;
                                       right_expr = Ast.Identifier "c";
                                     };
                               };
                           operator = Ast.Plus;
                           right_expr =
                             Ast.Infix
                               {
                                 left_expr = Ast.Identifier "d";
                                 operator = Ast.Slash;
                                 right_expr = Ast.Identifier "e";
                               };
                         };
                     operator = Ast.Minus;
                     right_expr = Ast.Identifier "f";
                   });
              Ast.Expr
                (Ast.Infix
                   {
                     left_expr =
                       Ast.Infix
                         {
                           left_expr = Ast.Integer 5;
                           operator = Ast.GT;
                           right_expr = Ast.Integer 4;
                         };
                     operator = Ast.Eq;
                     right_expr =
                       Ast.Infix
                         {
                           left_expr = Ast.Integer 3;
                           operator = Ast.LT;
                           right_expr = Ast.Integer 4;
                         };
                   });
              Ast.Expr
                (Ast.Infix
                   {
                     left_expr =
                       Ast.Infix
                         {
                           left_expr = Ast.Integer 5;
                           operator = Ast.LT;
                           right_expr = Ast.Integer 4;
                         };
                     operator = Ast.NEq;
                     right_expr =
                       Ast.Infix
                         {
                           left_expr = Ast.Integer 3;
                           operator = Ast.GT;
                           right_expr = Ast.Integer 4;
                         };
                   });
              Ast.Expr
                (Ast.Infix
                   {
                     left_expr =
                       Ast.Infix
                         {
                           left_expr = Ast.Integer 3;
                           operator = Ast.Plus;
                           right_expr =
                             Ast.Infix
                               {
                                 left_expr = Ast.Integer 4;
                                 operator = Ast.Asterisk;
                                 right_expr = Ast.Integer 5;
                               };
                         };
                     operator = Ast.Eq;
                     right_expr =
                       Ast.Infix
                         {
                           left_expr =
                             Ast.Infix
                               {
                                 left_expr = Ast.Integer 3;
                                 operator = Ast.Asterisk;
                                 right_expr = Ast.Integer 1;
                               };
                           operator = Ast.Plus;
                           right_expr =
                             Ast.Infix
                               {
                                 left_expr = Ast.Integer 4;
                                 operator = Ast.Asterisk;
                                 right_expr = Ast.Integer 5;
                               };
                         };
                   });
            ]))

let%test_unit "parse boolean literal expressions" =
  let inp = "true;false;" in
  [%test_result: Ast.program Or_error.t] (parse_str inp)
    ~expect:
      (Ok
         (Ast.Program
            [ Ast.Expr (Ast.Boolean true); Ast.Expr (Ast.Boolean false) ]))

let%test_unit "parse infix operator precedence with parenthesized expressions" =
  let inp =
    {|
      -( a*b )
      !( -a )
      a+( b-c )
      a*( b/c );
      ( a+b )/c;
      ( a + b ) * ( c + d ) / ( e - f );
      ( 3 + 4 ) * ( isBool( 5 == 3 ) * ( 1 + 4 ) * 5 );
    |}
  in
  [%test_result: Ast.program Or_error.t] (parse_str inp)
    ~expect:
      (Ok
         (Ast.Program
            [
              Ast.Expr
                (Ast.Prefix
                   {
                     operator = Ast.Minus;
                     expr =
                       Ast.Infix
                         {
                           left_expr = Ast.Identifier "a";
                           operator = Ast.Asterisk;
                           right_expr = Ast.Identifier "b";
                         };
                   });
              Ast.Expr
                (Ast.Prefix
                   {
                     operator = Ast.Bang;
                     expr =
                       Ast.Prefix
                         { operator = Ast.Minus; expr = Ast.Identifier "a" };
                   });
              Ast.Expr
                (Ast.Infix
                   {
                     left_expr = Ast.Identifier "a";
                     operator = Ast.Plus;
                     right_expr =
                       Ast.Infix
                         {
                           left_expr = Ast.Identifier "b";
                           operator = Ast.Minus;
                           right_expr = Ast.Identifier "c";
                         };
                   });
              Ast.Expr
                (Ast.Infix
                   {
                     left_expr = Ast.Identifier "a";
                     operator = Ast.Asterisk;
                     right_expr =
                       Ast.Infix
                         {
                           left_expr = Ast.Identifier "b";
                           operator = Ast.Slash;
                           right_expr = Ast.Identifier "c";
                         };
                   });
              Ast.Expr
                (Ast.Infix
                   {
                     left_expr =
                       Ast.Infix
                         {
                           left_expr = Ast.Identifier "a";
                           operator = Ast.Plus;
                           right_expr = Ast.Identifier "b";
                         };
                     operator = Ast.Slash;
                     right_expr = Ast.Identifier "c";
                   });
              Ast.Expr
                (Ast.Infix
                   {
                     left_expr =
                       Ast.Infix
                         {
                           left_expr =
                             Ast.Infix
                               {
                                 left_expr = Ast.Identifier "a";
                                 operator = Ast.Plus;
                                 right_expr = Ast.Identifier "b";
                               };
                           operator = Ast.Asterisk;
                           right_expr =
                             Ast.Infix
                               {
                                 left_expr = Ast.Identifier "c";
                                 operator = Ast.Plus;
                                 right_expr = Ast.Identifier "d";
                               };
                         };
                     operator = Ast.Slash;
                     right_expr =
                       Ast.Infix
                         {
                           left_expr = Ast.Identifier "e";
                           operator = Ast.Minus;
                           right_expr = Ast.Identifier "f";
                         };
                   });
              Ast.Expr
                (Ast.Infix
                   {
                     left_expr =
                       Ast.Infix
                         {
                           left_expr = Ast.Integer 3;
                           operator = Ast.Plus;
                           right_expr = Ast.Integer 4;
                         };
                     operator = Ast.Asterisk;
                     right_expr =
                       Ast.Infix
                         {
                           left_expr =
                             Ast.Infix
                               {
                                 left_expr =
                                   Ast.FnCall
                                     {
                                       func = Ast.Identifier "isBool";
                                       arguments =
                                         [
                                           Ast.Infix
                                             {
                                               left_expr = Ast.Integer 5;
                                               operator = Ast.Eq;
                                               right_expr = Ast.Integer 3;
                                             };
                                         ];
                                     };
                                 operator = Ast.Asterisk;
                                 right_expr =
                                   Ast.Infix
                                     {
                                       left_expr = Ast.Integer 1;
                                       operator = Ast.Plus;
                                       right_expr = Ast.Integer 4;
                                     };
                               };
                           operator = Ast.Asterisk;
                           right_expr = Ast.Integer 5;
                         };
                   });
            ]))

let%test_unit "parse if else expressions" =
  let inp =
    {|
      if (x < y) { x }
      if (x < y) { x } else { y }
    |}
  in
  [%test_result: Ast.program Or_error.t] (parse_str inp)
    ~expect:
      (Ok
         (Ast.Program
            [
              Ast.Expr
                (Ast.IfElseExpression
                   {
                     condition =
                       Ast.Infix
                         {
                           left_expr = Ast.Identifier "x";
                           operator = Ast.LT;
                           right_expr = Ast.Identifier "y";
                         };
                     consequence = Ast.Block [ Ast.Expr (Ast.Identifier "x") ];
                     alternative = None;
                   });
              Ast.Expr
                (Ast.IfElseExpression
                   {
                     condition =
                       Ast.Infix
                         {
                           left_expr = Ast.Identifier "x";
                           operator = Ast.LT;
                           right_expr = Ast.Identifier "y";
                         };
                     consequence = Ast.Block [ Ast.Expr (Ast.Identifier "x") ];
                     alternative =
                       Some (Ast.Block [ Ast.Expr (Ast.Identifier "y") ]);
                   });
            ]))

let%test_unit "parse function literals" =
  let inp =
    {|
      fn(x, y) { x+y; }
      fn() {42}
      fn(x) {x*x}
    |}
  in
  [%test_result: Ast.program Or_error.t] (parse_str inp)
    ~expect:
      (Ok
         (Ast.Program
            [
              Ast.Expr
                (Ast.Fn
                   {
                     parameters = [ Ast.Ident "x"; Ast.Ident "y" ];
                     body =
                       Ast.Block
                         [
                           Ast.Expr
                             (Ast.Infix
                                {
                                  left_expr = Ast.Identifier "x";
                                  operator = Ast.Plus;
                                  right_expr = Ast.Identifier "y";
                                });
                         ];
                   });
              Ast.Expr
                (Ast.Fn
                   {
                     parameters = [];
                     body = Ast.Block [ Ast.Expr (Ast.Integer 42) ];
                   });
              Ast.Expr
                (Ast.Fn
                   {
                     parameters = [ Ast.Ident "x" ];
                     body =
                       Ast.Block
                         [
                           Ast.Expr
                             (Ast.Infix
                                {
                                  left_expr = Ast.Identifier "x";
                                  operator = Ast.Asterisk;
                                  right_expr = Ast.Identifier "x";
                                });
                         ];
                   });
            ]))

let%test_unit "parse call expression" =
  let inp =
    {|
      add(1, 2 * 3, 4 + 5);
      fn(x, y) { let z = x + y; z }(2, 3);
      callsFunction(2, 3, fn(x, y) { x + y; });
    |}
  in
  [%test_result: Ast.program Or_error.t] (parse_str inp)
    ~expect:
      (Ok
         (Ast.Program
            [
              Ast.Expr
                (Ast.FnCall
                   {
                     func = Ast.Identifier "add";
                     arguments =
                       [
                         Ast.Integer 1;
                         Ast.Infix
                           {
                             left_expr = Ast.Integer 2;
                             operator = Ast.Asterisk;
                             right_expr = Ast.Integer 3;
                           };
                         Ast.Infix
                           {
                             left_expr = Ast.Integer 4;
                             operator = Ast.Plus;
                             right_expr = Ast.Integer 5;
                           };
                       ];
                   });
              Ast.Expr
                (Ast.FnCall
                   {
                     func =
                       Ast.Fn
                         {
                           parameters = [ Ast.Ident "x"; Ast.Ident "y" ];
                           body =
                             Ast.Block
                               [
                                 Ast.Let
                                   {
                                     name = Ast.Ident "z";
                                     value =
                                       Ast.Infix
                                         {
                                           left_expr = Ast.Identifier "x";
                                           operator = Ast.Plus;
                                           right_expr = Ast.Identifier "y";
                                         };
                                   };
                                 Ast.Expr (Ast.Identifier "z");
                               ];
                         };
                     arguments = [ Ast.Integer 2; Ast.Integer 3 ];
                   });
              Ast.Expr
                (Ast.FnCall
                   {
                     func = Ast.Identifier "callsFunction";
                     arguments =
                       [
                         Ast.Integer 2;
                         Ast.Integer 3;
                         Ast.Fn
                           {
                             parameters = [ Ast.Ident "x"; Ast.Ident "y" ];
                             body =
                               Ast.Block
                                 [
                                   Ast.Expr
                                     (Ast.Infix
                                        {
                                          left_expr = Ast.Identifier "x";
                                          operator = Ast.Plus;
                                          right_expr = Ast.Identifier "y";
                                        });
                                 ];
                           };
                       ];
                   });
            ]))
