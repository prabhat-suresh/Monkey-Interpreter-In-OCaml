open Base

let start ic oc =
  Out_channel.output_string oc ">> ";
  Out_channel.flush oc;
  In_channel.fold_lines
    (fun () line ->
      Lexer.lex line
      |> List.iter ~f:(fun tok ->
          Out_channel.output_string oc @@ Sexp.to_string @@ Token.sexp_of_t tok;
          Out_channel.output_char oc '\n');
      Out_channel.output_string oc ">> ";
      Out_channel.flush oc)
    () ic
