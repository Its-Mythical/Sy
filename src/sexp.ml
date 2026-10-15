(* S-expression reader.  This is SY's uniform core form (Axis 1: "uniform core
   form always reachable").  Every node carries a source position. *)

type pos = { line : int; col : int }

type t =
  | Atom of string * pos
  | Int of int * pos
  | Str of string * pos
  | List of t list * pos

exception Syntax_error of pos * string

let pos_of = function
  | Atom (_, p) | Int (_, p) | Str (_, p) | List (_, p) -> p

let is_delim c =
  match c with
  | ' ' | '\t' | '\r' | '\n' | '(' | ')' | ';' | '"' -> true
  | _ -> false

let is_int_literal s =
  let n = String.length s in
  let start = if n > 1 && s.[0] = '-' then 1 else 0 in
  n > start
  &&
  let ok = ref true in
  for i = start to n - 1 do
    if s.[i] < '0' || s.[i] > '9' then ok := false
  done;
  !ok

let parse_all (src : string) : t list =
  let n = String.length src in
  let i = ref 0 and line = ref 1 and col = ref 1 in
  let peek () = if !i < n then Some src.[!i] else None in
  let adv () =
    if src.[!i] = '\n' then (
      incr line;
      col := 1)
    else incr col;
    incr i
  in
  let rec skip () =
    match peek () with
    | Some (' ' | '\t' | '\r' | '\n') ->
        adv ();
        skip ()
    | Some ';' ->
        while !i < n && src.[!i] <> '\n' do
          adv ()
        done;
        skip ()
    | _ -> ()
  in
  let rec read_expr () : t =
    skip ();
    let pos = { line = !line; col = !col } in
    match peek () with
    | None -> raise (Syntax_error (pos, "unexpected end of input"))
    | Some ')' -> raise (Syntax_error (pos, "unexpected ')'"))
    | Some '(' ->
        adv ();
        let rec items acc =
          skip ();
          match peek () with
          | None -> raise (Syntax_error (pos, "unclosed '('"))
          | Some ')' ->
              adv ();
              List.rev acc
          | _ -> items (read_expr () :: acc)
        in
        List (items [], pos)
    | Some '"' ->
        adv ();
        let b = Buffer.create 16 in
        let rec go () =
          match peek () with
          | None -> raise (Syntax_error (pos, "unterminated string literal"))
          | Some '"' -> adv ()
          | Some '\\' ->
              adv ();
              (match peek () with
              | Some 'n' -> Buffer.add_char b '\n'
              | Some 't' -> Buffer.add_char b '\t'
              | Some '"' -> Buffer.add_char b '"'
              | Some '\\' -> Buffer.add_char b '\\'
              | _ ->
                  raise
                    (Syntax_error
                       ({ line = !line; col = !col }, "unknown string escape")));
              adv ();
              go ()
          | Some c ->
              Buffer.add_char b c;
              adv ();
              go ()
        in
        go ();
        Str (Buffer.contents b, pos)
    | Some _ ->
        let start = !i in
        while !i < n && not (is_delim src.[!i]) do
          adv ()
        done;
        let tok = String.sub src start (!i - start) in
        if is_int_literal tok then Int (int_of_string tok, pos)
        else Atom (tok, pos)
  in
  let rec top acc =
    skip ();
    if !i >= n then List.rev acc else top (read_expr () :: acc)
  in
  top []
