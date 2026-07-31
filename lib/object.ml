open Base

(* separate constructors for true and false to not allocate multiple instances *)
(* of true and false each time, as there is just one value of each that should *)
(* be referenced globally *)
type t = Integer of int64 | True | False | Null | Return of t | Err of string
[@@deriving compare, sexp]

let native_bool_to_boolean_object b = if b then True else False
let is_truthy = function False | Null -> false | _ -> true

let type_of = function
  | True | False -> "BOOLEAN"
  | Null -> "NULL"
  | Integer _ -> "INTEGER"
  | _ -> failwith "These types shouldn't be operated upon"
