(* Types, effect rows and unification (§11).

   Types are Hindley–Milner with levels (Rémy-style generalization).  Effect
   rows are Koka/Leijen-style scoped rows: a row is a list of effect labels
   ending in either the empty row or a row variable.  Function types carry a
   *latent* row: the effects performed when the function is called. *)

type ty =
  | TCon of string * ty list
  | TFun of ty list * row * ty
  | TVar of tv ref

and row =
  | REmpty
  | RExt of string * row
  | RVar of rv ref

and tv = TUnbound of int * int (* id, level *) | TLink of ty
and rv = RUnbound of int * int | RLink of row

exception Type_error of string

let err fmt = Printf.ksprintf (fun s -> raise (Type_error s)) fmt
let generic_level = max_int
let current_level = ref 0
let enter_level () = incr current_level
let leave_level () = decr current_level
let counter = ref 0

let fresh_id () =
  incr counter;
  !counter

let new_tvar () = TVar (ref (TUnbound (fresh_id (), !current_level)))
let new_rvar () = RVar (ref (RUnbound (fresh_id (), !current_level)))
let tint = TCon ("Int", [])
let tstr = TCon ("Str", [])

let rec repr t =
  match t with
  | TVar ({ contents = TLink t' } as r) ->
      let t'' = repr t' in
      r := TLink t'';
      t''
  | _ -> t

let rec rrepr r =
  match r with
  | RVar ({ contents = RLink r' } as v) ->
      let r'' = rrepr r' in
      v := RLink r'';
      r''
  | _ -> r

let rec row_of_labels labels tail =
  match labels with [] -> tail | l :: rest -> RExt (l, row_of_labels rest tail)

(* ---- printing ------------------------------------------------------- *)

let show_ty_with (t : ty) : string =
  let tnames = Hashtbl.create 8 and rnames = Hashtbl.create 8 in
  let rcount = Hashtbl.create 8 in
  let bump id =
    Hashtbl.replace rcount id (1 + try Hashtbl.find rcount id with Not_found -> 0)
  in
  let rec count_ty t =
    match repr t with
    | TCon (_, a) -> List.iter count_ty a
    | TFun (a, r, b) ->
        List.iter count_ty a;
        count_row r;
        count_ty b
    | TVar _ -> ()
  and count_row r =
    match rrepr r with
    | REmpty -> ()
    | RExt (_, r) -> count_row r
    | RVar { contents = RUnbound (id, _) } -> bump id
    | RVar _ -> ()
  in
  count_ty t;
  let tname id =
    match Hashtbl.find_opt tnames id with
    | Some n -> n
    | None ->
        let i = Hashtbl.length tnames in
        let n =
          if i < 26 then String.make 1 (Char.chr (97 + i))
          else Printf.sprintf "t%d" i
        in
        Hashtbl.add tnames id n;
        n
  in
  let rname id =
    match Hashtbl.find_opt rnames id with
    | Some n -> n
    | None ->
        let n = Printf.sprintf "e%d" (Hashtbl.length rnames + 1) in
        Hashtbl.add rnames id n;
        n
  in
  let rec pp t =
    match repr t with
    | TCon (n, []) -> n
    | TCon (n, args) -> "(" ^ String.concat " " (n :: List.map pp_arg args) ^ ")"
    | TVar { contents = TUnbound (id, _) } -> tname id
    | TVar _ -> assert false
    | TFun (args, r, ret) ->
        "(" ^ String.concat " " (List.map pp_arg args) ^ ") -> " ^ pp_row r ^ pp ret
  and pp_arg t = match repr t with TFun _ -> "(" ^ pp t ^ ")" | _ -> pp t
  and pp_row r =
    let rec collect r acc =
      match rrepr r with
      | RExt (l, r') -> collect r' (l :: acc)
      | REmpty -> (List.rev acc, None)
      | RVar { contents = RUnbound (id, _) } -> (List.rev acc, Some id)
      | RVar _ -> assert false
    in
    let labels, tail = collect r [] in
    let shared id = (try Hashtbl.find rcount id with Not_found -> 0) > 1 in
    match (labels, tail) with
    | [], None -> ""
    | [], Some id -> if shared id then "<" ^ rname id ^ "> " else ""
    | ls, Some id when shared id ->
        "<" ^ String.concat "," ls ^ "|" ^ rname id ^ "> "
    | ls, _ -> "<" ^ String.concat "," ls ^ "> "
  in
  pp t

let show_ty t = show_ty_with t

let show_row r =
  let s = show_ty_with (TFun ([], r, tint)) in
  (* "() -> <..> Int" -> "<..>" *)
  let s = String.sub s 6 (String.length s - 6) in
  let s = String.sub s 0 (String.length s - 4) in
  if s = "" then "<>" else String.trim s

(* ---- unification ---------------------------------------------------- *)

let rec adjust_ty id lvl t =
  match repr t with
  | TCon (_, args) -> List.iter (adjust_ty id lvl) args
  | TFun (args, r, ret) ->
      List.iter (adjust_ty id lvl) args;
      adjust_row lvl r;
      adjust_ty id lvl ret
  | TVar ({ contents = TUnbound (id', l) } as r) ->
      if id' = id then err "cannot construct an infinite type"
      else if l > lvl then r := TUnbound (id', lvl)
  | TVar _ -> assert false

and adjust_row lvl r =
  match rrepr r with
  | REmpty -> ()
  | RExt (_, r') -> adjust_row lvl r'
  | RVar ({ contents = RUnbound (id, l) } as v) -> if l > lvl then v := RUnbound (id, lvl)
  | RVar _ -> assert false

(* lower every free variable of t to at most lvl (no occurs check) *)
let rec lower_ty lvl t =
  match repr t with
  | TCon (_, args) -> List.iter (lower_ty lvl) args
  | TFun (args, r, ret) ->
      List.iter (lower_ty lvl) args;
      adjust_row lvl r;
      lower_ty lvl ret
  | TVar ({ contents = TUnbound (id, l) } as r) -> if l > lvl && l <> generic_level then r := TUnbound (id, lvl)
  | TVar _ -> ()

let rec row_tail r =
  match rrepr r with RExt (_, r') -> row_tail r' | t -> t

let rec rewrite_row l r =
  match rrepr r with
  | RExt (l', t) when l' = l -> Some t
  | RExt (l', t) -> (
      match rewrite_row l t with Some t' -> Some (RExt (l', t')) | None -> None)
  | RVar ({ contents = RUnbound (_, lvl) } as v) ->
      let fresh = RVar (ref (RUnbound (fresh_id (), lvl))) in
      v := RLink (RExt (l, fresh));
      Some fresh
  | REmpty -> None
  | RVar _ -> assert false

let rec unify_row r1 r2 =
  let r1 = rrepr r1 and r2 = rrepr r2 in
  match (r1, r2) with
  | REmpty, REmpty -> ()
  | RVar a, RVar b when a == b -> ()
  | RVar ({ contents = RUnbound (id, lvl) } as a), r | r, RVar ({ contents = RUnbound (id, lvl) } as a) ->
      (match row_tail r with
      | RVar { contents = RUnbound (id', _) } when id' = id ->
          err "effect rows are recursive (%s)" (show_row r)
      | _ -> ());
      adjust_row lvl r;
      a := RLink r
  | RExt (l, t), other -> (
      match rewrite_row l other with
      | Some rest -> unify_row t rest
      | None ->
          err "effect `%s` is not available here (the enclosing context only allows %s)" l
            (show_row other))
  | REmpty, RExt (l, _) ->
      err "effect `%s` is not available here (the enclosing context only allows %s)" l
        (show_row REmpty)
  | _ -> err "internal error: malformed effect row"

let rec unify t1 t2 =
  let t1 = repr t1 and t2 = repr t2 in
  if t1 == t2 then ()
  else
    match (t1, t2) with
    | TVar { contents = TUnbound (a, _) }, TVar { contents = TUnbound (b, _) } when a = b -> ()
    | TVar ({ contents = TUnbound (id, lvl) } as r), t | t, TVar ({ contents = TUnbound (id, lvl) } as r) ->
        adjust_ty id lvl t;
        r := TLink t
    | TCon (a, xs), TCon (b, ys) when a = b && List.length xs = List.length ys ->
        List.iter2 unify xs ys
    | TFun (a1, r1, b1), TFun (a2, r2, b2) when List.length a1 = List.length a2 ->
        List.iter2 unify a1 a2;
        unify_row r1 r2;
        unify b1 b2
    | _ -> err "cannot unify `%s` with `%s`" (show_ty t1) (show_ty t2)

(* ---- generalization ------------------------------------------------- *)

let rec generalize t =
  match repr t with
  | TCon (_, args) -> List.iter generalize args
  | TFun (args, r, ret) ->
      List.iter generalize args;
      generalize_row r;
      generalize ret
  | TVar ({ contents = TUnbound (id, l) } as r) ->
      if l > !current_level then r := TUnbound (id, generic_level)
  | TVar _ -> ()

and generalize_row r =
  match rrepr r with
  | REmpty -> ()
  | RExt (_, r') -> generalize_row r'
  | RVar ({ contents = RUnbound (id, l) } as v) ->
      if l > !current_level then v := RUnbound (id, generic_level)
  | RVar _ -> ()

let instantiate t =
  let tt = Hashtbl.create 8 and rt = Hashtbl.create 8 in
  let rec inst t =
    match repr t with
    | TVar { contents = TUnbound (id, l) } when l = generic_level -> (
        match Hashtbl.find_opt tt id with
        | Some v -> v
        | None ->
            let v = new_tvar () in
            Hashtbl.add tt id v;
            v)
    | TVar _ as t -> t
    | TCon (n, args) -> TCon (n, List.map inst args)
    | TFun (a, r, b) -> TFun (List.map inst a, inst_row r, inst b)
  and inst_row r =
    match rrepr r with
    | REmpty -> REmpty
    | RExt (l, r') -> RExt (l, inst_row r')
    | RVar { contents = RUnbound (id, l) } when l = generic_level -> (
        match Hashtbl.find_opt rt id with
        | Some v -> v
        | None ->
            let v = new_rvar () in
            Hashtbl.add rt id v;
            v)
    | r -> r
  in
  inst t
