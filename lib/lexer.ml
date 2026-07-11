open Base

let lex inp =
  String.fold inp ~init:[] ~f:(fun tokens ch ->
      let tok =
        match ch with
        | '=' -> Token.Assign
        | ';' -> Semicolon
        | '(' -> LParen
        | ')' -> RParen
        | ',' -> Comma
        | '+' -> Plus
        | '{' -> LBrace
        | '}' -> RBrace
        | _ -> Illegal
      in
      tok :: tokens)
  |> List.rev

let%test_unit "lexer" =
  let tokens = lex "=+(){},;" in
  [%test_eq: Token.t list] tokens
    [ Assign; Plus; LParen; RParen; LBrace; RBrace; Comma; Semicolon ]
