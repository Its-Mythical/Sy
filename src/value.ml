(* Runtime values.

   Evaluation returns a `result`: either a finished value, or a suspended
   effect operation together with its resumption.  A handler is just a
   function over results, which is what makes the resumption an ordinary
   (and therefore multi-shot capable) OCaml closure. *)

type value =
  | VInt of int
  | VStr of string
  | VCon of string * value list
  | VFun of int * (value list -> result)

and result =
  | Ret of value
  | Op of string * value list * (value -> result)

exception Runtime_error of string

let rec show = function
  | VInt n -> string_of_int n
  | VStr s -> Printf.sprintf "%S" s
  | VCon (c, []) -> c
  | VCon (c, vs) -> "(" ^ String.concat " " (c :: List.map show vs) ^ ")"
  | VFun (n, _) -> Printf.sprintf "<fn/%d>" n
