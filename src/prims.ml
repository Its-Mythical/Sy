(* Built-in primitives.  Their types are written in SY's own type syntax and
   parsed by the same reader as everything else.  `(-> (Int Int) Int)` means
   "pure"; effects, e.g. print's `io`, are listed explicitly. *)

open Value

type prim = { name : string; ty : string; arity : int; impl : value list -> value }

let unit_v = VCon ("Unit", [])
let bool_v b = VCon ((if b then "True" else "False"), [])

let int2 name f =
  { name; ty = "(-> (Int Int) Int)"; arity = 2;
    impl = (function [ VInt a; VInt b ] -> VInt (f a b) | _ -> raise (Runtime_error name)) }

let cmp name f =
  { name; ty = "(-> (Int Int) Bool)"; arity = 2;
    impl = (function [ VInt a; VInt b ] -> bool_v (f a b) | _ -> raise (Runtime_error name)) }

let table ~(out : string -> unit) : prim list =
  [
    int2 "+" ( + );
    int2 "-" ( - );
    int2 "*" ( * );
    int2 "/" (fun a b -> if b = 0 then raise (Runtime_error "division by zero") else a / b);
    int2 "%" (fun a b -> if b = 0 then raise (Runtime_error "division by zero") else a mod b);
    cmp "==" ( = );
    cmp "!=" ( <> );
    cmp "<" ( < );
    cmp "<=" ( <= );
    cmp ">" ( > );
    cmp ">=" ( >= );
    { name = "++"; ty = "(-> (Str Str) Str)"; arity = 2;
      impl = (function [ VStr a; VStr b ] -> VStr (a ^ b) | _ -> raise (Runtime_error "++")) };
    { name = "str="; ty = "(-> (Str Str) Bool)"; arity = 2;
      impl = (function [ VStr a; VStr b ] -> bool_v (a = b) | _ -> raise (Runtime_error "str=")) };
    { name = "str-len"; ty = "(-> (Str) Int)"; arity = 1;
      impl = (function [ VStr a ] -> VInt (String.length a) | _ -> raise (Runtime_error "str-len")) };
    { name = "int->str"; ty = "(-> (Int) Str)"; arity = 1;
      impl = (function [ VInt a ] -> VStr (string_of_int a) | _ -> raise (Runtime_error "int->str")) };
    { name = "print"; ty = "(-> (Str) (io) Unit)"; arity = 1;
      impl = (function [ VStr s ] -> out s; unit_v | _ -> raise (Runtime_error "print")) };
    { name = "println"; ty = "(-> (Str) (io) Unit)"; arity = 1;
      impl = (function [ VStr s ] -> out (s ^ "\n"); unit_v | _ -> raise (Runtime_error "println")) };
  ]
