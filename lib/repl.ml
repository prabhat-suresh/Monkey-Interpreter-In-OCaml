open Base

let start ic oc =
  let rec loop () =
    Out_channel.output_string oc ">> ";
    Out_channel.flush oc;
    match In_channel.input_line ic with
    | None -> ()
    | Some line ->
        Lexer.lex line |> Parser.parse
        |> Or_error.sexp_of_t Ast.sexp_of_program
        |> Sexp.to_string_hum
        |> Out_channel.output_string oc;
        Out_channel.output_string oc "\n";
        loop ()
  in
  loop ()
