(* S-expression -> core AST.

   The only surface sugar in Milestone 1 is the part the design already calls
   "a macro over the seven forms":
     (if c a b)    => Case c { True -> a ; False -> b }
     (do e1 .. en) => Let _ = e1 in ... en
   Everything else maps one-to-one onto a core form. *)

open Ast
open Sexp

let err p m = raise (Syntax_error (p, m))

let reserved =
  [ "fn"; "let"; "letrec"; "case"; "handle"; "if"; "do"; "return"; "def";
    "defrec"; "data"; "effect"; "->" ]

let is_upper s = s <> "" && s.[0] >= 'A' && s.[0] <= 'Z'

let var_name = function
  | Atom (a, p) ->
      if List.mem a reserved then
        err p (Printf.sprintf "`%s` is a keyword and cannot be used as a name" a)
      else if is_upper a then
        err p (Printf.sprintf "`%s`: variable names must start with a lowercase letter" a)
      else a
  | s -> err (pos_of s) "expected a name"

let rec pat (s : Sexp.t) : pat =
  match s with
  | Int (n, _) -> PLit (LInt n)
  | Str (x, _) -> PLit (LStr x)
  | Atom ("_", _) -> PWild
  | Atom (a, p) ->
      if is_upper a then PCon (a, [])
      else if List.mem a reserved then err p "keyword in pattern position"
      else PVar a
  | List (Atom (c, _) :: args, _) when is_upper c -> PCon (c, List.map pat args)
  | List ([], p) -> err p "empty pattern"
  | s -> err (pos_of s) "malformed pattern (constructor patterns look like (Cons x xs))"

let rec expr (s : Sexp.t) : expr =
  match s with
  | Int (n, _) -> Lit (LInt n)
  | Str (x, _) -> Lit (LStr x)
  | Atom (a, p) ->
      if List.mem a reserved then
        err p (Printf.sprintf "`%s` is a keyword and cannot be used as a value" a)
      else Var a
  | List ([], p) -> err p "empty application `()`"
  | List (Atom ("fn", p) :: rest, _) -> (
      match rest with
      | [ List (params, _); body ] ->
          Lam (List.map (fun s -> match s with Atom ("_", _) -> "_" | s -> var_name s) params, expr body)
      | _ -> err p "expected (fn (params...) body)")
  | List (Atom (("let" | "letrec") as kw, p) :: rest, _) -> (
      match rest with
      | [ x; e1; e2 ] -> Let (kw = "letrec", var_name x, expr e1, expr e2)
      | _ -> err p (Printf.sprintf "expected (%s name rhs body)" kw))
  | List (Atom ("if", p) :: rest, _) -> (
      match rest with
      | [ c; a; b ] ->
          Case (expr c, [ (PCon ("True", []), expr a); (PCon ("False", []), expr b) ])
      | _ -> err p "expected (if cond then else)")
  | List (Atom ("do", p) :: rest, _) ->
      if rest = [] then err p "(do ...) needs at least one expression"
      else
        let rec seq = function
          | [ e ] -> expr e
          | e :: tl -> Let (false, "_", expr e, seq tl)
          | [] -> assert false
        in
        seq rest
  | List (Atom ("case", p) :: rest, _) -> (
      match rest with
      | scrut :: clauses when clauses <> [] ->
          Case
            ( expr scrut,
              List.map
                (function
                  | List ([ p; b ], _) -> (pat p, expr b)
                  | s -> err (pos_of s) "expected a clause (pattern body)")
                clauses )
      | _ -> err p "expected (case scrutinee (pattern body) ...)")
  | List (Atom ("handle", p) :: rest, _) -> (
      match rest with
      | body :: clauses ->
          let ops = ref [] and ret = ref None in
          List.iter
            (function
              | List ([ Atom ("return", rp); List ([ x ], _); rb ], _) ->
                  if !ret <> None then err rp "duplicate return clause";
                  ret := Some (var_name x, expr rb)
              | List ([ Atom (op, op_p); List (params, pp); hb ], _) ->
                  if is_upper op || List.mem op reserved then
                    err op_p "expected an effect operation name";
                  if params = [] then
                    err pp "handler clause needs a resumption parameter: (op (args... k) body)";
                  let names = List.map var_name params in
                  let rec split = function
                    | [ k ] -> ([], k)
                    | x :: tl ->
                        let a, k = split tl in
                        (x :: a, k)
                    | [] -> assert false
                  in
                  let args, k = split names in
                  ops := (op, args, k, expr hb) :: !ops
              | s ->
                  err (pos_of s)
                    "expected a handler clause: (op (args... k) body) or (return (x) body)")
            clauses;
          Handle (expr body, { ops = List.rev !ops; ret = !ret })
      | [] -> err p "expected (handle body clause...)")
  | List (f :: args, _) -> App (expr f, List.map expr args)

(* ---- types ---------------------------------------------------------- *)

let atom_name = function
  | Atom (a, _) -> a
  | s -> err (pos_of s) "expected an effect name"

let rec sty (s : Sexp.t) : sty =
  match s with
  | Atom (a, p) ->
      if List.mem a reserved then err p "keyword in type position"
      else if is_upper a then STCon (a, [])
      else STVar a
  | List (Atom ("->", _) :: rest, p) -> (
      match rest with
      | [ List (args, _); ret ] -> STFun (List.map sty args, [], sty ret)
      | [ List (args, _); List (effs, _); ret ] ->
          STFun (List.map sty args, List.map atom_name effs, sty ret)
      | _ -> err p "expected (-> (args...) result) or (-> (args...) (effects...) result)")
  | List (Atom (c, _) :: args, _) when is_upper c -> STCon (c, List.map sty args)
  | s -> err (pos_of s) "malformed type"

(* ---- declarations --------------------------------------------------- *)

let decl (s : Sexp.t) : decl =
  match s with
  | List (Atom (("def" | "defrec") as kw, p) :: rest, _) -> (
      match rest with
      | [ x; e ] -> DDef (kw = "defrec", var_name x, expr e)
      | _ -> err p (Printf.sprintf "expected (%s name expr)" kw))
  | List (Atom ("data", p) :: rest, _) -> (
      match rest with
      | head :: ctors ->
          let name, params =
            match head with
            | Atom (n, _) when is_upper n -> (n, [])
            | List (Atom (n, _) :: ps, _) when is_upper n -> (n, List.map var_name ps)
            | s -> err (pos_of s) "expected a type name or (Name params...)"
          in
          let ctors =
            List.map
              (function
                | List (Atom (c, _) :: fields, _) when is_upper c -> (c, List.map sty fields)
                | s -> err (pos_of s) "expected a constructor (Name fields...)")
              ctors
          in
          DData (name, params, ctors)
      | [] -> err p "expected (data Name constructors...)")
  | List (Atom ("effect", p) :: rest, _) -> (
      match rest with
      | Atom (name, _) :: ops ->
          let ops =
            List.map
              (function
                | List ([ Atom (o, _); List (args, _); res ], _) -> (o, List.map sty args, sty res)
                | s -> err (pos_of s) "expected an operation (name (arg types...) result)")
              ops
          in
          DEffect (name, ops)
      | _ -> err p "expected (effect name operations...)")
  | s -> DExpr (expr s)

let program (src : string) : (Sexp.pos * decl) list =
  List.map (fun s -> (pos_of s, decl s)) (Sexp.parse_all src)
