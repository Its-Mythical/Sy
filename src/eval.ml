(* Reference evaluator for the seven core forms.

   Strict, call-by-value.  Effects are free-monad style: `Op` results bubble up
   through `bind` until a Handle clause captures them.  Handlers are *deep*:
   the resumption re-installs the handler, so `k v` behaves like the rest of
   the handled computation.  Because a resumption is a plain closure, calling
   it twice (multi-shot) just works — this is the semantics-first interpreter,
   not the fast one (Invariant 2 specialization belongs to the backend). *)

open Ast
open Value
module SMap = Map.Make (String)

type env = value ref SMap.t

let rec bind (m : result) (f : value -> result) : result =
  match m with
  | Ret v -> f v
  | Op (o, args, k) -> Op (o, args, fun v -> bind (k v) f)

let extend env names vals =
  List.fold_left2 (fun m x v -> SMap.add x (ref v) m) env names vals

let rec match_pat p v : (string * value) list option =
  match (p, v) with
  | PWild, _ -> Some []
  | PVar x, v -> Some [ (x, v) ]
  | PLit (LInt n), VInt m -> if n = m then Some [] else None
  | PLit (LStr s), VStr t -> if s = t then Some [] else None
  | PCon (c, ps), VCon (c', vs) when c = c' && List.length ps = List.length vs ->
      List.fold_left2
        (fun acc p v ->
          match acc with
          | None -> None
          | Some a -> ( match match_pat p v with None -> None | Some b -> Some (a @ b)))
        (Some []) ps vs
  | _ -> None

let rec eval (env : env) (e : expr) : result =
  match e with
  | Var x -> (
      match SMap.find_opt x env with
      | Some r -> Ret !r
      | None -> raise (Runtime_error ("unbound variable " ^ x)))
  | Lit (LInt n) -> Ret (VInt n)
  | Lit (LStr s) -> Ret (VStr s)
  | Lam (xs, body) ->
      Ret (VFun (List.length xs, fun args -> eval (extend env xs args) body))
  | App (f, args) ->
      bind (eval env f) (fun fv -> eval_list env args (fun vs -> apply fv vs))
  | Let (false, x, e1, e2) -> bind (eval env e1) (fun v -> eval (extend env [ x ] [ v ]) e2)
  | Let (true, x, e1, e2) ->
      let cell = ref (VInt 0) in
      let env' = SMap.add x cell env in
      bind (eval env' e1) (fun v ->
          cell := v;
          eval env' e2)
  | Case (scrut, branches) ->
      bind (eval env scrut) (fun v ->
          let rec go = function
            | [] -> raise (Runtime_error ("no case branch matches " ^ show v))
            | (p, body) :: rest -> (
                match match_pat p v with
                | Some binds ->
                    eval (extend env (List.map fst binds) (List.map snd binds)) body
                | None -> go rest)
          in
          go branches)
  | Handle (body, h) ->
      let rec run (m : result) : result =
        match m with
        | Ret v -> (
            match h.ret with
            | Some (x, rb) -> eval (extend env [ x ] [ v ]) rb
            | None -> Ret v)
        | Op (o, args, k) -> (
            match List.find_opt (fun (op, _, _, _) -> op = o) h.ops with
            | Some (_, params, kname, hbody) ->
                let resume = VFun (1, function [ v ] -> run (k v) | _ -> raise (Runtime_error "resume")) in
                eval (extend env (params @ [ kname ]) (args @ [ resume ])) hbody
            | None -> Op (o, args, fun v -> run (k v)))
      in
      run (eval env body)

and eval_list env es (k : value list -> result) : result =
  match es with
  | [] -> k []
  | e :: rest -> bind (eval env e) (fun v -> eval_list env rest (fun vs -> k (v :: vs)))

and apply fv vs : result =
  match fv with
  | VFun (n, f) ->
      if n = List.length vs then f vs
      else
        raise
          (Runtime_error
             (Printf.sprintf "function expects %d argument(s), got %d" n (List.length vs)))
  | v -> raise (Runtime_error ("not a function: " ^ show v))

(* Run to completion at the top level: nothing handles stray operations. *)
let run_top env e : value =
  match eval env e with
  | Ret v -> v
  | Op (o, _, _) -> raise (Runtime_error ("unhandled effect operation `" ^ o ^ "`"))
