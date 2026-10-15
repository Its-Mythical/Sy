(* Printing the core AST back as S-expressions (`sy core`). *)

open Ast

let lit = function LInt n -> string_of_int n | LStr s -> Printf.sprintf "%S" s

let rec pat = function
  | PWild -> "_"
  | PVar x -> x
  | PLit l -> lit l
  | PCon (c, []) -> c
  | PCon (c, ps) -> "(" ^ String.concat " " (c :: List.map pat ps) ^ ")"

let rec expr = function
  | Var x -> x
  | Lit l -> lit l
  | Lam (xs, b) -> Printf.sprintf "(fn (%s) %s)" (String.concat " " xs) (expr b)
  | App (f, args) -> "(" ^ String.concat " " (expr f :: List.map expr args) ^ ")"
  | Let (r, x, a, b) ->
      Printf.sprintf "(%s %s %s %s)" (if r then "letrec" else "let") x (expr a) (expr b)
  | Case (e, bs) ->
      Printf.sprintf "(case %s %s)" (expr e)
        (String.concat " "
           (List.map (fun (p, b) -> Printf.sprintf "(%s %s)" (pat p) (expr b)) bs))
  | Handle (e, h) ->
      let ops =
        List.map
          (fun (op, xs, k, b) ->
            Printf.sprintf "(%s (%s) %s)" op (String.concat " " (xs @ [ k ])) (expr b))
          h.ops
      in
      let ret =
        match h.ret with
        | Some (x, b) -> [ Printf.sprintf "(return (%s) %s)" x (expr b) ]
        | None -> []
      in
      Printf.sprintf "(handle %s %s)" (expr e) (String.concat " " (ops @ ret))

let rec sty = function
  | STVar a -> a
  | STCon (c, []) -> c
  | STCon (c, args) -> "(" ^ String.concat " " (c :: List.map sty args) ^ ")"
  | STFun (args, [], r) ->
      Printf.sprintf "(-> (%s) %s)" (String.concat " " (List.map sty args)) (sty r)
  | STFun (args, effs, r) ->
      Printf.sprintf "(-> (%s) (%s) %s)" (String.concat " " (List.map sty args))
        (String.concat " " effs) (sty r)

let decl = function
  | DDef (r, x, e) -> Printf.sprintf "(%s %s %s)" (if r then "defrec" else "def") x (expr e)
  | DExpr e -> expr e
  | DData (n, ps, cs) ->
      let head = if ps = [] then n else "(" ^ String.concat " " (n :: ps) ^ ")" in
      let c (name, fs) = "(" ^ String.concat " " (name :: List.map sty fs) ^ ")" in
      Printf.sprintf "(data %s %s)" head (String.concat " " (List.map c cs))
  | DEffect (n, ops) ->
      let o (name, args, res) =
        Printf.sprintf "(%s (%s) %s)" name (String.concat " " (List.map sty args)) (sty res)
      in
      Printf.sprintf "(effect %s %s)" n (String.concat " " (List.map o ops))
