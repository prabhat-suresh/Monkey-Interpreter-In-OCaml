open Base

let start ic oc =
  let rec loop () =
    Out_channel.output_string oc ">> ";
    Out_channel.flush oc;
    match In_channel.input_line ic with
    | None -> ()
    | Some line ->
        let output =
          match Parser.parse (Lexer.lex line) with
          | Error e -> Error.to_string_hum e
          | Ok obj -> (
              match Evaluator.eval obj with
              | Object.Integer n -> Int64.to_string_hum n
              | True -> "true"
              | False -> "false"
              | Null -> "NULL")
        in
        Out_channel.output_string oc output;
        Out_channel.output_string oc "\n";
        loop ()
  in
  loop ()
