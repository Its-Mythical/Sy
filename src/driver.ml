(* Ties the pipeline together: read -> desugar -> infer -> evaluate. *)

open Ast
module SMap = Eval.SMap

exception Sy_error of Sexp.pos * string

type mode = Run | Check

type session = {
  ctx : Infer.ctx;
  mutable venv : Eval.env;
  out : string -> unit;
}

let define_value s name v = s.venv <- SMap.add name (ref v) s.venv

let declare_runtime s = function
  | DData (_, _, ctors) ->
      List.iter
        (fun (c, fields) ->
          let n = List.length fields in
          let v =
            if n = 0 then Value.VCon (c, [])
            else Value.VFun (n, fun args -> Value.Ret (Value.VCon (c, args)))
          in
          define_value s c v)
        ctors
  | DEffect (_, ops) ->
      List.iter
        (fun (o, args, _) ->
          define_value s o
            (Value.VFun (List.length args, fun a -> Value.Op (o, a, fun v -> Value.Ret v))))
        ops
  | _ -> ()

(* Phase 1: type/effect-check one declaration, updating the type context.
   Returns the line `check` mode prints, and the runtime action to perform
   later.  Nothing is executed here. *)
let check_decl s (d : decl) : string option * (unit -> string option) =
  match d with
  | DData (name, params, ctors) ->
      Infer.declare_data s.ctx name params ctors;
      (None, fun () -> declare_runtime s d; None)
  | DEffect (name, ops) ->
      Infer.declare_effect s.ctx name ops;
      (None, fun () -> declare_runtime s d; None)
  | DDef (is_rec, name, e) ->
      let t = Infer.infer_def s.ctx is_rec name e in
      ( Some (Printf.sprintf "%s : %s" name (Types.show_ty t)),
        fun () ->
          if is_rec then (
            let cell = ref (Value.VInt 0) in
            s.venv <- SMap.add name cell s.venv;
            cell := Eval.run_top s.venv e)
          else define_value s name (Eval.run_top s.venv e);
          None )
  | DExpr e ->
      let t = Infer.infer_top s.ctx e in
      ( Some (Printf.sprintf ": %s" (Types.show_ty t)),
        fun () ->
          match Eval.run_top s.venv e with
          | Value.VCon ("Unit", []) -> None
          | v -> Some (Value.show v) )

let describe_decl = function
  | DDef (_, n, _) -> Printf.sprintf " in `%s`" n
  | _ -> ""

let guard_check pos d f =
  try f () with
  | Types.Type_error m ->
      raise (Sy_error (pos, Printf.sprintf "type error%s: %s" (describe_decl d) m))

let guard_run pos f =
  try f () with
  | Value.Runtime_error m -> raise (Sy_error (pos, "runtime error: " ^ m))
  | Stack_overflow ->
      raise (Sy_error (pos, "runtime error: stack overflow (try `ulimit -s unlimited`)"))

(* check + run one declaration immediately (used to boot the prelude) *)
let exec_now s pos d =
  let _, action = guard_check pos d (fun () -> check_decl s d) in
  ignore (guard_run pos action)

let make_session ?(out = print_string) () : session =
  let s = { ctx = Infer.create (); venv = SMap.empty; out } in
  let boot_prims = Prims.table ~out in
  (* the prelude declares Unit/Bool/... which prim *types* mention, so declare
     the data/effect parts first, then register prims, then the definitions *)
  let prelude = Parse.program Prelude.source in
  let is_decl = function DData _ | DEffect _ -> true | _ -> false in
  List.iter (fun (p, d) -> if is_decl d then exec_now s p d) prelude;
  List.iter
    (fun (p : Prims.prim) ->
      let ty = Parse.sty (List.hd (Sexp.parse_all p.ty)) in
      Infer.add_prim s.ctx p.name ty;
      define_value s p.name (Value.VFun (p.arity, fun args -> Value.Ret (p.impl args))))
    boot_prims;
  List.iter (fun (p, d) -> if not (is_decl d) then exec_now s p d) prelude;
  s

(* Parse and process a whole program, emitting output through s.out.

   Run mode is two-phase: the *entire* program is type- and effect-checked
   first, so a type error anywhere means nothing runs and nothing is printed. *)
let run_source s ~mode (src : string) : unit =
  let prog =
    try Parse.program src
    with Sexp.Syntax_error (p, m) -> raise (Sy_error (p, "syntax error: " ^ m))
  in
  let checked =
    List.map
      (fun (pos, d) ->
        let line, action = guard_check pos d (fun () -> check_decl s d) in
        (pos, line, action))
      prog
  in
  match mode with
  | Check -> List.iter (fun (_, line, _) -> Option.iter (fun l -> s.out (l ^ "\n")) line) checked
  | Run ->
      List.iter
        (fun (pos, _, action) ->
          match guard_run pos action with Some l -> s.out (l ^ "\n") | None -> ())
        checked
