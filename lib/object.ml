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

and env = {
  store : ((string, t) Hashtbl.t[@sexp.opaque] [@compare.ignore]);
  outer : env option;
}
[@@deriving compare, sexp]

let of_bool b = if b then True else False
let is_truthy = function False | Null -> false | _ -> true

let type_of = function
  | True | False -> "BOOLEAN"
  | Null -> "NULL"
  | Integer _ -> "INTEGER"
  | Return _ -> "RETURN"
  | Err _ -> "ERROR"
  | Function _ -> "FUNCTION"

module Environment = struct
  type t = env

  let new_environment () =
    { store = Hashtbl.create (module String); outer = None }

  let new_enclosed_environment outer =
    { store = Hashtbl.create (module String); outer = Some outer }

  let rec get env name =
    match Hashtbl.find env.store name with
    | None -> Option.bind env.outer ~f:(fun env -> get env name)
    | obj -> obj

  let set env = Hashtbl.set env.store
end
