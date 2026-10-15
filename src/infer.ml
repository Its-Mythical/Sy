(* Type + effect inference over the seven core forms (§03, §11, §12).

   `infer ctx env eff e` infers the type of e *assuming the ambient effect row
   is eff*: every call unifies the callee's latent row with eff, so effects
   flow upward through Lam (which opens a fresh row for its body) and are
   discharged by Handle (which types its body under <labels | eff>).
   Purity falls out as "empty residual row". *)

open Ast
open Types
module SMap = Map.Make (String)

type ctor_info = { tyname : string; arity : int; siblings : (string * int) list }

type ctx = {
  ctors : (string, ctor_info) Hashtbl.t;
  tycons : (string, int) Hashtbl.t;
  effects : (string, string list) Hashtbl.t; (* label -> operations *)
  ops : (string, string) Hashtbl.t; (* operation -> label *)
  optys : (string, ty) Hashtbl.t; (* operation -> generalized signature *)
  mutable env : ty SMap.t;
}

let create () =
  let tycons = Hashtbl.create 16 in
  Hashtbl.replace tycons "Int" 0;
  Hashtbl.replace tycons "Str" 0;
  {
    ctors = Hashtbl.create 16;
    tycons;
    effects = Hashtbl.create 16;
    ops = Hashtbl.create 16;
    optys = Hashtbl.create 16;
    env = SMap.empty;
  }

(* ---- surface types -> internal types -------------------------------- *)

let rec conv ctx tbl ~open_vars (s : sty) : ty =
  match s with
  | STVar n -> (
      match Hashtbl.find_opt tbl n with
      | Some t -> t
      | None ->
          if open_vars then (
            let t = new_tvar () in
            Hashtbl.add tbl n t;
            t)
          else err "unbound type variable `%s`" n)
  | STCon (n, args) -> (
      match Hashtbl.find_opt ctx.tycons n with
      | None -> err "unknown type `%s`" n
      | Some k ->
          if k <> List.length args then
            err "type `%s` expects %d argument(s), got %d" n k (List.length args)
          else TCon (n, List.map (conv ctx tbl ~open_vars) args))
  | STFun (args, effs, ret) ->
      List.iter
        (fun l -> if not (Hashtbl.mem ctx.effects l) then err "unknown effect `%s`" l)
        effs;
      TFun
        ( List.map (conv ctx tbl ~open_vars) args,
          row_of_labels effs (new_rvar ()),
          conv ctx tbl ~open_vars ret )

(* ---- declarations ---------------------------------------------------- *)

let add_prim ctx name (s : sty) =
  enter_level ();
  let t = conv ctx (Hashtbl.create 4) ~open_vars:true s in
  leave_level ();
  generalize t;
  ctx.env <- SMap.add name t ctx.env

let declare_data ctx name params ctors =
  if Hashtbl.mem ctx.tycons name then err "type `%s` is already defined" name;
  Hashtbl.replace ctx.tycons name (List.length params);
  enter_level ();
  let tbl = Hashtbl.create 4 in
  let pvars =
    List.map
      (fun n ->
        let t = new_tvar () in
        Hashtbl.replace tbl n t;
        t)
      params
  in
  let res = TCon (name, pvars) in
  let siblings = List.map (fun (c, fs) -> (c, List.length fs)) ctors in
  let tys =
    List.map
      (fun (c, fields) ->
        if Hashtbl.mem ctx.ctors c then err "constructor `%s` is already defined" c;
        let fts = List.map (conv ctx tbl ~open_vars:false) fields in
        (c, List.length fields, if fields = [] then res else TFun (fts, new_rvar (), res)))
      ctors
  in
  leave_level ();
  List.iter
    (fun (c, arity, t) ->
      generalize t;
      Hashtbl.replace ctx.ctors c { tyname = name; arity; siblings };
      ctx.env <- SMap.add c t ctx.env)
    tys

let declare_effect ctx label ops =
  if Hashtbl.mem ctx.effects label then err "effect `%s` is already defined" label;
  Hashtbl.replace ctx.effects label (List.map (fun (o, _, _) -> o) ops);
  List.iter
    (fun (o, args, res) ->
      if Hashtbl.mem ctx.ops o then err "effect operation `%s` is already defined" o;
      enter_level ();
      let tbl = Hashtbl.create 4 in
      let args' = List.map (conv ctx tbl ~open_vars:true) args in
      let res' = conv ctx tbl ~open_vars:true res in
      let t = TFun (args', RExt (label, new_rvar ()), res') in
      leave_level ();
      generalize t;
      Hashtbl.replace ctx.ops o label;
      Hashtbl.replace ctx.optys o t;
      ctx.env <- SMap.add o t ctx.env)
    ops

(* ---- pattern exhaustiveness (Maranget) ------------------------------- *)

let rec take n l = if n = 0 then [] else match l with x :: r -> x :: take (n - 1) r | [] -> []
let rec drop n l = if n = 0 then l else match l with _ :: r -> drop (n - 1) r | [] -> []

let specialize c a rows =
  List.filter_map
    (function
      | PCon (c', qs) :: rest when c' = c -> Some (qs @ rest)
      | (PWild | PVar _) :: rest -> Some (List.init a (fun _ -> PWild) @ rest)
      | _ -> None)
    rows

let default_rows rows =
  List.filter_map (function (PWild | PVar _) :: rest -> Some rest | _ -> None) rows

let rec witness ctx rows n : pat list option =
  if n = 0 then if rows = [] then Some [] else None
  else
    let heads = List.filter_map (function PCon (c, _) :: _ -> Some c | _ -> None) rows in
    match heads with
    | c0 :: _ ->
        let info = Hashtbl.find ctx.ctors c0 in
        let used = List.sort_uniq compare heads in
        if List.for_all (fun (c, _) -> List.mem c used) info.siblings then
          let rec try_ = function
            | [] -> None
            | (c, a) :: rest -> (
                match witness ctx (specialize c a rows) (a + n - 1) with
                | Some w -> Some (PCon (c, take a w) :: drop a w)
                | None -> try_ rest)
          in
          try_ info.siblings
        else (
          match witness ctx (default_rows rows) (n - 1) with
          | None -> None
          | Some w ->
              let c, a = List.find (fun (c, _) -> not (List.mem c used)) info.siblings in
              Some (PCon (c, List.init a (fun _ -> PWild)) :: w))
    | [] -> (
        match witness ctx (default_rows rows) (n - 1) with
        | None -> None
        | Some w -> Some (PWild :: w))

let check_exhaustive ctx branches =
  match witness ctx (List.map (fun (p, _) -> [ p ]) branches) 1 with
  | Some [ w ] -> err "non-exhaustive match: the pattern `%s` is not covered" (Pretty.pat w)
  | _ -> ()

(* ---- inference ------------------------------------------------------- *)

let rec is_value = function
  | Lam _ | Lit _ | Var _ -> true
  | App (Var c, args) when Parse.is_upper c -> List.for_all is_value args
  | _ -> false

let describe = function
  | Var x -> "`" ^ x ^ "`"
  | _ -> "this expression"

let with_context msg f =
  try f () with Type_error m -> raise (Type_error (Printf.sprintf "%s: %s" msg m))

let lookup env x =
  match SMap.find_opt x env with
  | Some t -> instantiate t
  | None -> err "unbound variable `%s`" x

let rec infer ctx env (eff : row) (e : expr) : ty =
  match e with
  | Var x -> lookup env x
  | Lit (LInt _) -> tint
  | Lit (LStr _) -> tstr
  | Lam (xs, body) ->
      let ps = List.map (fun _ -> new_tvar ()) xs in
      let r = new_rvar () in
      let env' = List.fold_left2 (fun m x t -> SMap.add x t m) env xs ps in
      let t = infer ctx env' r body in
      TFun (ps, r, t)
  | App (f, args) -> (
      let tf = infer ctx env eff f in
      let targs = List.map (infer ctx env eff) args in
      let name = describe f in
      match repr tf with
      | TFun (ps, r, ret) ->
          if List.length ps <> List.length targs then
            err "%s expects %d argument(s) but is called with %d" name (List.length ps)
              (List.length targs);
          List.iteri
            (fun i (p, a) ->
              with_context (Printf.sprintf "argument %d of %s" (i + 1) name) (fun () -> unify p a))
            (List.combine ps targs);
          with_context (Printf.sprintf "calling %s" name) (fun () -> unify_row r eff);
          ret
      | TVar _ ->
          let ret = new_tvar () in
          with_context (Printf.sprintf "calling %s" name) (fun () ->
              unify tf (TFun (targs, eff, ret)));
          ret
      | t -> err "cannot call %s: it has type `%s`, which is not a function" name (show_ty t))
  | Let (false, x, e1, e2) ->
      enter_level ();
      let t1 = infer ctx env eff e1 in
      leave_level ();
      if is_value e1 then generalize t1 else lower_ty !current_level t1;
      infer ctx (SMap.add x t1 env) eff e2
  | Let (true, x, e1, e2) ->
      (match e1 with Lam _ -> () | _ -> err "`letrec %s` must bind a function (fn ...)" x);
      enter_level ();
      let tv = new_tvar () in
      let t1 = infer ctx (SMap.add x tv env) eff e1 in
      with_context (Printf.sprintf "recursive definition of `%s`" x) (fun () -> unify tv t1);
      leave_level ();
      generalize t1;
      infer ctx (SMap.add x t1 env) eff e2
  | Case (scrut, branches) ->
      let ts = infer ctx env eff scrut in
      let result = new_tvar () in
      List.iteri
        (fun i (p, body) ->
          let binds = infer_pat ctx p ts [] in
          let env' = List.fold_left (fun m (x, t) -> SMap.add x t m) env binds in
          let tb = infer ctx env' eff body in
          with_context (Printf.sprintf "branch %d of case" (i + 1)) (fun () -> unify result tb))
        branches;
      check_exhaustive ctx branches;
      result
  | Handle (body, h) ->
      let label_of op =
        match Hashtbl.find_opt ctx.ops op with
        | Some l -> l
        | None -> err "unknown effect operation `%s` in handler" op
      in
      let handled = List.map (fun (op, _, _, _) -> op) h.ops in
      List.iteri
        (fun i op ->
          if List.mem op (drop (i + 1) handled) then err "operation `%s` is handled twice" op)
        handled;
      let labels =
        List.fold_left
          (fun acc op ->
            let l = label_of op in
            if List.mem l acc then acc else acc @ [ l ])
          [] handled
      in
      List.iter
        (fun l ->
          let missing =
            List.filter (fun o -> not (List.mem o handled)) (Hashtbl.find ctx.effects l)
          in
          if missing <> [] then
            err "a handler for effect `%s` must handle all of its operations; missing: %s" l
              (String.concat ", " missing))
        labels;
      let body_row = row_of_labels labels eff in
      let tb = infer ctx env body_row body in
      let tr =
        match h.ret with
        | Some (x, rb) -> infer ctx (SMap.add x tb env) eff rb
        | None -> tb
      in
      List.iter
        (fun (op, params, k, hbody) ->
          match instantiate (Hashtbl.find ctx.optys op) with
          | TFun (ps, _, res) ->
              if List.length ps <> List.length params then
                err "handler clause `%s` takes %d argument(s) plus the resumption, got %d" op
                  (List.length ps) (List.length params);
              let env' = List.fold_left2 (fun m x t -> SMap.add x t m) env params ps in
              let env' = SMap.add k (TFun ([ res ], eff, tr)) env' in
              let th = infer ctx env' eff hbody in
              with_context (Printf.sprintf "handler clause `%s`" op) (fun () -> unify tr th)
          | _ -> assert false)
        h.ops;
      tr

and infer_pat ctx p t acc : (string * ty) list =
  match p with
  | PWild -> acc
  | PVar x ->
      if List.mem_assoc x acc then err "variable `%s` is bound twice in one pattern" x;
      (x, t) :: acc
  | PLit (LInt _) ->
      unify t tint;
      acc
  | PLit (LStr _) ->
      unify t tstr;
      acc
  | PCon (c, ps) -> (
      match Hashtbl.find_opt ctx.ctors c with
      | None -> err "unknown constructor `%s`" c
      | Some info -> (
          if info.arity <> List.length ps then
            err "constructor `%s` has %d field(s) but the pattern gives %d" c info.arity
              (List.length ps);
          match instantiate (SMap.find c ctx.env) with
          | TFun (fts, _, res) ->
              unify t res;
              List.fold_left2 (fun acc p ft -> infer_pat ctx p ft acc) acc ps fts
          | res ->
              unify t res;
              acc))

(* Ambient row for top-level code: only the world (`io`) is available. *)
let top_row = RExt ("io", REmpty)

let infer_def ctx is_rec name e : ty =
  enter_level ();
  let t =
    if is_rec then (
      (match e with Lam _ -> () | _ -> err "`defrec %s` must bind a function (fn ...)" name);
      let tv = new_tvar () in
      let t = infer ctx (SMap.add name tv ctx.env) top_row e in
      with_context (Printf.sprintf "recursive definition of `%s`" name) (fun () -> unify tv t);
      t)
    else infer ctx ctx.env top_row e
  in
  leave_level ();
  if is_value e then generalize t else lower_ty !current_level t;
  ctx.env <- SMap.add name t ctx.env;
  t

let infer_top ctx e : ty =
  enter_level ();
  let t = infer ctx ctx.env top_row e in
  leave_level ();
  lower_ty !current_level t;
  t
