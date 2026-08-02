open Base

(* separate constructors for true and false to not allocate multiple instances *)
(* of true and false each time, as there is just one value of each that should *)
(* be referenced globally *)
type t =
  | Integer of int64
  | True
  | False
  | Null
  | Return of t
  | Err of string
  | Function of { fn : Ast.function_expression; env : env }

and env = ((string, t) Hashtbl.t[@sexp.opaque] [@compare.ignore])
[@@deriving compare, sexp]

let of_bool b = if b then True else False
let is_truthy = function False | Null -> false | _ -> true

let type_of = function
  | True | False -> "BOOLEAN"
  | Null -> "NULL"
  | Integer _ -> "INTEGER"
  | _ -> failwith "These types shouldn't be operated upon"

module Environment = struct
  type t = env

  let new_environment () = Hashtbl.create (module String)
  let get = Hashtbl.find
  let set = Hashtbl.set
end
