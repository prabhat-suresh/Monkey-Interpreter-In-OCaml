open Base

(* separate constructors for true and false to not allocate multiple instances *)
(* of true and false each time, as there is just one value of each that should *)
(* be referenced globally *)
type t = Integer of int64 | True | False | Null [@@deriving compare, sexp]

let native_bool_to_boolean_object b = if b then True else False
