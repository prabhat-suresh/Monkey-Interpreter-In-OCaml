open Base

let is_letter c = Char.(c = '_' || is_alpha c)

let rec token_helper acc ~f = function
  | [] -> (List.rev acc |> String.of_char_list, [])
  | hd :: tl as l ->
      if f hd then token_helper (hd :: acc) ~f tl
      else (List.rev acc |> String.of_char_list, l)

let read_identifier_or_keyword inp =
  let identifier, rest = token_helper [] ~f:is_letter inp in
  let identifier_or_keyword =
    match identifier with
    | "let" -> Token.Let
    | "fn" -> Token.Function
    | "true" -> True
    | "false" -> False
    | "if" -> If
    | "else" -> Else
    | "return" -> Return
    | _ -> Token.Ident identifier
  in
  (rest, identifier_or_keyword)

let read_number inp =
  let number, rest = token_helper [] ~f:Char.is_digit inp in
  (rest, Token.Int (Int.of_string number))

let lex inp =
  let rec helper tokens = function
    | [] -> tokens
    | hd :: tl as l ->
        if Char.is_whitespace hd then helper tokens tl
        else
          let rest, tok =
            if is_letter hd then read_identifier_or_keyword l
            else if Char.is_digit hd then read_number l
            else
              match hd with
              | '=' -> (
                  match tl with
                  | '=' :: tl' -> (tl', Token.Eq)
                  | _ -> (tl, Token.Assign))
              | '!' -> (
                  match tl with
                  | '=' :: tl' -> (tl', Token.NEq)
                  | _ -> (tl, Token.Bang))
              | ';' -> (tl, Semicolon)
              | '(' -> (tl, LParen)
              | ')' -> (tl, RParen)
              | ',' -> (tl, Comma)
              | '+' -> (tl, Plus)
              | '-' -> (tl, Minus)
              | '*' -> (tl, Asterisk)
              | '/' -> (tl, Slash)
              | '<' -> (tl, LT)
              | '>' -> (tl, GT)
              | '{' -> (tl, LBrace)
              | '}' -> (tl, RBrace)
              | _ -> (tl, Illegal)
          in
          helper (tok :: tokens) rest
  in
  helper [] (String.to_list inp) |> List.rev

let%test_unit "lexer" =
  let inp =
    {|
    let five = 5;
    let ten = 10;
    let add = fn(x, y) {
        x + y;
    };
    let result = add(five, ten);
    !-/*5;
    5 < 10 > 5;

    if (5 < 10) {
        return true;
    } else {
        return false;
    }

    10 == 10;
    10 != 9;
  |}
  in
  let tokens = lex inp in
  [%test_eq: Token.t list] tokens
    [
      Let;
      Ident "five";
      Assign;
      Int 5;
      Semicolon;
      Let;
      Ident "ten";
      Assign;
      Int 10;
      Semicolon;
      Let;
      Ident "add";
      Assign;
      Function;
      LParen;
      Ident "x";
      Comma;
      Ident "y";
      RParen;
      LBrace;
      Ident "x";
      Plus;
      Ident "y";
      Semicolon;
      RBrace;
      Semicolon;
      Let;
      Ident "result";
      Assign;
      Ident "add";
      LParen;
      Ident "five";
      Comma;
      Ident "ten";
      RParen;
      Semicolon;
      Bang;
      Minus;
      Slash;
      Asterisk;
      Int 5;
      Semicolon;
      Int 5;
      LT;
      Int 10;
      GT;
      Int 5;
      Semicolon;
      If;
      LParen;
      Int 5;
      LT;
      Int 10;
      RParen;
      LBrace;
      Return;
      True;
      Semicolon;
      RBrace;
      Else;
      LBrace;
      Return;
      False;
      Semicolon;
      RBrace;
      Int 10;
      Eq;
      Int 10;
      Semicolon;
      Int 10;
      NEq;
      Int 9;
      Semicolon;
    ]
