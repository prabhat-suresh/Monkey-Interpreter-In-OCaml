open Base

let start ic oc =
  Out_channel.output_string oc ">> ";
  Out_channel.flush oc;
  In_channel.fold_lines
    (fun () line ->
      Lexer.lex line |> Parser.parse
      |> Or_error.sexp_of_t Ast.sexp_of_program
      |> Sexp.to_string_hum
      |> Out_channel.output_string oc;
      Out_channel.output_string oc "\n>> ";
      Out_channel.flush oc)
    () ic
