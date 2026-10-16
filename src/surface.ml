(* The indentation-significant surface syntax (Axis 1, default lane).

   Pipeline: text -> layout-aware tokens (INDENT / DEDENT / NEWLINE) ->
   recursive-descent + precedence-climbing parser -> core AST.  There is no
   separate surface AST: every construct is desugared while parsing, exactly as
   the design says ("every surface form lowers to the seven core forms").
   docs/SURFACE.md has the grammar and the desugaring table. *)

open Ast
open Sexp

let err p m = raise (Syntax_error (p, m))

type tok =
  | INT of int
  | STR of string
  | ID of string
  | CON of string
  | SYM of string
  | NEWLINE
  | INDENT
  | DEDENT
  | EOF

type token = { t : tok; pos : pos }

let show_tok = function
  | INT n -> string_of_int n
  | STR s -> Printf.sprintf "%S" s
  | ID s | CON s -> "`" ^ s ^ "`"
  | SYM s -> "`" ^ s ^ "`"
  | NEWLINE -> "end of line"
  | INDENT -> "an indented block"
  | DEDENT -> "the end of a block"
  | EOF -> "end of input"

let keywords =
  [ "fn"; "let"; "data"; "effect"; "match"; "handle"; "if"; "then"; "else"; "return" ]

(* a line ending in one of these continues on the next line *)
let continues_line = function
  | SYM ("+" | "-" | "*" | "/" | "%" | "++" | "==" | "!=" | "<=" | ">=" | "&&" | "||" | "|>" | ",") -> true
  | _ -> false

(* ---- lexer ----------------------------------------------------------- *)

let lex (src : string) : token array =
  let n = String.length src in
  let toks = ref [] in
  let emit t pos = toks := { t; pos } :: !toks in
  let indents = ref [ 0 ] in
  let depth = ref 0 in
  let i = ref 0 and line = ref 1 and lstart = ref 0 in
  let pos () = { line = !line; col = !i - !lstart + 1 } in
  let at_line_start = ref true in
  let peek_at k = if !i + k < n then Some src.[!i + k] else None in
  let is_id_start c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' in
  let is_id_char c = is_id_start c || (c >= '0' && c <= '9') in
  let is_digit c = c >= '0' && c <= '9' in
  while !i < n do
    if !at_line_start then (
      at_line_start := false;
      let start = !i in
      while !i < n && src.[!i] = ' ' do
        incr i
      done;
      if !i < n && src.[!i] = '\t' then
        err (pos ()) "tabs are not allowed for indentation; use spaces";
      let col = !i - start in
      let blank = !i >= n || src.[!i] = '\n' || src.[!i] = '\r' || src.[!i] = '#' in
      if (not blank) && !depth = 0 then (
        let top = List.hd !indents in
        let prev_continues = match !toks with tk :: _ -> continues_line tk.t | [] -> false in
        let starts_pipe = !i + 1 < n && src.[!i] = '|' && src.[!i + 1] = '>' in
        if prev_continues || (starts_pipe && col > top) then ()
        else if col > top then (
          indents := col :: !indents;
          emit INDENT (pos ()))
        else if col < top then (
          while List.hd !indents > col do
            indents := List.tl !indents;
            emit DEDENT (pos ())
          done;
          if List.hd !indents <> col then
            err (pos ()) "inconsistent indentation: this line does not line up with any enclosing block";
          emit NEWLINE (pos ()))
        else if !toks <> [] then emit NEWLINE (pos ())))
    else
      let c = src.[!i] in
      let p = pos () in
      if c = ' ' || c = '\r' then incr i
      else if c = '\t' then incr i
      else if c = '\n' then (
        incr i;
        incr line;
        lstart := !i;
        at_line_start := true)
      else if c = '#' then
        while !i < n && src.[!i] <> '\n' do
          incr i
        done
      else if is_digit c then (
        let s = !i in
        while !i < n && is_digit src.[!i] do
          incr i
        done;
        emit (INT (int_of_string (String.sub src s (!i - s)))) p)
      else if is_id_start c then (
        let s = !i in
        while !i < n && is_id_char src.[!i] do
          incr i
        done;
        let w = String.sub src s (!i - s) in
        emit (if c >= 'A' && c <= 'Z' then CON w else ID w) p)
      else if c = '"' then (
        incr i;
        let b = Buffer.create 16 in
        let rec go () =
          if !i >= n || src.[!i] = '\n' then err p "unterminated string literal"
          else
            match src.[!i] with
            | '"' -> incr i
            | '\\' ->
                (match peek_at 1 with
                | Some 'n' -> Buffer.add_char b '\n'
                | Some 't' -> Buffer.add_char b '\t'
                | Some '"' -> Buffer.add_char b '"'
                | Some '\\' -> Buffer.add_char b '\\'
                | _ -> err (pos ()) "unknown string escape");
                i := !i + 2;
                go ()
            | ch ->
                Buffer.add_char b ch;
                incr i;
                go ()
        in
        go ();
        emit (STR (Buffer.contents b)) p)
      else
        let two = if !i + 1 < n then String.sub src !i 2 else "" in
        match two with
        | "->" | "==" | "!=" | "<=" | ">=" | "++" | "&&" | "||" | "|>" ->
            i := !i + 2;
            emit (SYM two) p
        | _ -> (
            match c with
            | '(' ->
                incr depth;
                incr i;
                emit (SYM "(") p
            | ')' ->
                decr depth;
                if !depth < 0 then err p "unmatched ')'";
                incr i;
                emit (SYM ")") p
            | ',' | '=' | '|' | '<' | '>' | '+' | '-' | '*' | '/' | '%' | ':' ->
                incr i;
                emit (SYM (String.make 1 c)) p
            | _ -> err p (Printf.sprintf "unexpected character %C" c))
  done;
  if !depth > 0 then err (pos ()) "unclosed '('";
  let p = pos () in
  List.iter (fun _ -> emit DEDENT p) (List.filter (fun x -> x > 0) !indents);
  emit EOF p;
  Array.of_list (List.rev !toks)

(* ---- parser ---------------------------------------------------------- *)

type stmt =
  | SLet of string * expr
  | SFn of string * string list * expr
  | SExpr of expr

type st = { toks : token array; mutable p : int }

let cur s = s.toks.(s.p)
let peek s = (cur s).t
let peek2 s = if s.p + 1 < Array.length s.toks then s.toks.(s.p + 1).t else EOF
let peek3 s = if s.p + 2 < Array.length s.toks then s.toks.(s.p + 2).t else EOF
let adv s = if s.p < Array.length s.toks - 1 then s.p <- s.p + 1
let fail s msg = err (cur s).pos msg
let unexpected s what = fail s (Printf.sprintf "expected %s, found %s" what (show_tok (peek s)))

let expect_sym s x =
  if peek s = SYM x then adv s else unexpected s (Printf.sprintf "`%s`" x)

let expect_id s kw =
  if peek s = ID kw then adv s else unexpected s (Printf.sprintf "`%s`" kw)

let is_kw x = List.mem x keywords

let ident s what =
  match peek s with
  | ID x when not (is_kw x) ->
      adv s;
      x
  | _ -> unexpected s what

let tru = PCon ("True", [])
let fls = PCon ("False", [])

let pipe lhs rhs =
  match rhs with App (f, args) -> App (f, lhs :: args) | f -> App (f, [ lhs ])

let binop_prec = function
  | "|>" -> Some 1
  | "||" -> Some 2
  | "&&" -> Some 3
  | "==" | "!=" | "<" | "<=" | ">" | ">=" -> Some 4
  | "+" | "-" | "++" -> Some 5
  | "*" | "/" | "%" -> Some 6
  | _ -> None

let combine op l r =
  match op with
  | "|>" -> pipe l r
  | "&&" -> Case (l, [ (tru, r); (fls, Var "False") ])
  | "||" -> Case (l, [ (tru, Var "True"); (fls, r) ])
  | _ -> App (Var op, [ l; r ])

(* comma-separated list between parentheses; the opening paren is current *)
let paren_list s (item : st -> 'a) : 'a list =
  expect_sym s "(";
  if peek s = SYM ")" then (
    adv s;
    [])
  else
    let rec go acc =
      let x = item s in
      match peek s with
      | SYM "," ->
          adv s;
          go (x :: acc)
      | SYM ")" ->
          adv s;
          List.rev (x :: acc)
      | _ -> unexpected s "`,` or `)`"
    in
    go []

let rec parse_type s : sty =
  match peek s with
  | CON c ->
      adv s;
      if peek s = SYM "(" then STCon (c, paren_list s parse_type) else STCon (c, [])
  | ID a when not (is_kw a) ->
      adv s;
      STVar a
  | SYM "(" ->
      let args = paren_list s parse_type in
      expect_sym s "->";
      let effs =
        if peek s = SYM "<" then (
          adv s;
          let rec go acc =
            let l = ident s "an effect name" in
            match peek s with
            | SYM "," ->
                adv s;
                go (l :: acc)
            | SYM ">" ->
                adv s;
                List.rev (l :: acc)
            | _ -> unexpected s "`,` or `>`"
          in
          go [])
        else []
      in
      STFun (args, effs, parse_type s)
  | _ -> unexpected s "a type"

let rec parse_pat s : pat =
  match peek s with
  | INT n ->
      adv s;
      PLit (LInt n)
  | SYM "-" when (match peek2 s with INT _ -> true | _ -> false) -> (
      adv s;
      match peek s with
      | INT n ->
          adv s;
          PLit (LInt (-n))
      | _ -> assert false)
  | STR x ->
      adv s;
      PLit (LStr x)
  | ID "_" ->
      adv s;
      PWild
  | ID x when not (is_kw x) ->
      adv s;
      PVar x
  | CON c ->
      adv s;
      if peek s = SYM "(" then PCon (c, paren_list s parse_pat) else PCon (c, [])
  | _ -> unexpected s "a pattern"

(* a body is either an expression on the same line or an indented block *)
let rec parse_body s : expr =
  if peek s = INDENT then (
    adv s;
    parse_block s)
  else parse_expr s

and parse_block s : expr =
  (* INDENT already consumed *)
  let rec go acc =
    let st = parse_stmt s in
    match peek s with
    | NEWLINE ->
        adv s;
        go (st :: acc)
    | DEDENT ->
        adv s;
        List.rev (st :: acc)
    | _ -> unexpected s "end of line"
  in
  let start = (cur s).pos in
  let stmts = go [] in
  let rec build = function
    | [ SExpr e ] -> e
    | [ _ ] -> err start "a block must end with an expression (its value)"
    | SLet (x, e) :: rest -> Let (false, x, e, build rest)
    | SFn (f, ps, b) :: rest -> Let (true, f, Lam (ps, b), build rest)
    | SExpr e :: rest -> Let (false, "_", e, build rest)
    | [] -> assert false
  in
  build stmts

and parse_stmt s : stmt =
  match peek s with
  | ID "let" ->
      adv s;
      let x = ident s "a name" in
      expect_sym s "=";
      SLet (x, parse_body s)
  | ID "fn" when (match (peek2 s, peek3 s) with ID _, SYM "(" -> true | _ -> false) ->
      adv s;
      let name, params, body = parse_fn_rest s in
      SFn (name, params, body)
  | ID ("data" | "effect") -> fail s "`data` and `effect` declarations are only allowed at the top level"
  | _ -> SExpr (parse_expr s)

and parse_fn_rest s : string * string list * expr =
  (* after `fn`, at NAME ( params ) = body *)
  let name = ident s "a function name" in
  let params =
    paren_list s (fun s ->
        let x = ident s "a parameter name" in
        if peek s = SYM ":" then
          fail s "type annotations are not supported yet (types are fully inferred in this milestone)";
        x)
  in
  if peek s = SYM "->" then
    fail s "return-type annotations are not supported yet (types are fully inferred in this milestone)";
  expect_sym s "=";
  (name, params, parse_body s)

and parse_expr s : expr = parse_bin s 1

and parse_bin s minp : expr =
  let lhs = ref (parse_unary s) in
  let rec loop () =
    match peek s with
    | SYM op -> (
        match binop_prec op with
        | Some p when p >= minp ->
            adv s;
            let rhs = parse_bin s (p + 1) in
            lhs := combine op !lhs rhs;
            loop ()
        | _ -> ())
    | _ -> ()
  in
  loop ();
  !lhs

and parse_unary s : expr =
  match peek s with
  | SYM "-" -> (
      adv s;
      match parse_unary s with
      | Lit (LInt n) -> Lit (LInt (-n))
      | e -> App (Var "-", [ Lit (LInt 0); e ]))
  | _ -> parse_postfix s

and parse_postfix s : expr =
  let e = ref (parse_atom s) in
  while peek s = SYM "(" do
    e := App (!e, paren_list s parse_expr)
  done;
  !e

and parse_atom s : expr =
  match peek s with
  | INT n ->
      adv s;
      Lit (LInt n)
  | STR x ->
      adv s;
      Lit (LStr x)
  | CON c ->
      adv s;
      Var c
  | SYM "(" ->
      adv s;
      if peek s = SYM ")" then (
        adv s;
        Var "Unit")
      else
        let e = parse_expr s in
        expect_sym s ")";
        e
  | ID "fn" -> (
      adv s;
      let params =
        match peek s with
        | SYM "(" -> paren_list s (fun s -> ident s "a parameter name")
        | ID _ -> [ ident s "a parameter name" ]
        | _ -> unexpected s "lambda parameters"
      in
      expect_sym s "->";
      Lam (params, parse_body s))
  | ID "if" ->
      adv s;
      let c = parse_expr s in
      expect_id s "then";
      let a = parse_body s in
      if peek s = NEWLINE && peek2 s = ID "else" then adv s;
      expect_id s "else";
      let b = parse_body s in
      Case (c, [ (tru, a); (fls, b) ])
  | ID "match" ->
      adv s;
      let scrut = parse_expr s in
      let arms =
        parse_clauses s (fun s ->
            let p = parse_pat s in
            expect_sym s "->";
            (p, parse_body s))
      in
      Case (scrut, arms)
  | ID "handle" ->
      adv s;
      let body = parse_expr s in
      let clauses =
        parse_clauses s (fun s ->
            let at = (cur s).pos in
            match peek s with
            | ID "return" ->
                adv s;
                let x = (match paren_list s (fun s -> ident s "a name") with
                         | [ x ] -> x
                         | _ -> err at "a return clause takes exactly one parameter: return(x) -> ...")
                in
                expect_sym s "->";
                `Ret (at, x, parse_body s)
            | ID op when not (is_kw op) ->
                adv s;
                let names = paren_list s (fun s -> ident s "a name") in
                if names = [] then
                  err at "a handler clause needs a resumption parameter: op(args..., k) -> ...";
                expect_sym s "->";
                let hb = parse_body s in
                let rec split = function
                  | [ k ] -> ([], k)
                  | x :: tl ->
                      let a, k = split tl in
                      (x :: a, k)
                  | [] -> assert false
                in
                let args, k = split names in
                `Op (op, args, k, hb)
            | _ -> unexpected s "a handler clause")
      in
      let ret = ref None and ops = ref [] in
      List.iter
        (function
          | `Ret (at, x, b) ->
              if !ret <> None then err at "duplicate return clause";
              ret := Some (x, b)
          | `Op c -> ops := c :: !ops)
        clauses;
      Handle (body, { ops = List.rev !ops; ret = !ret })
  | ID "let" -> fail s "`let` is only allowed as a statement (at the start of a line)"
  | ID x when is_kw x -> fail s (Printf.sprintf "unexpected keyword `%s`" x)
  | ID x ->
      adv s;
      Var x
  | _ -> unexpected s "an expression"

(* an indented list of clauses, one per line *)
and parse_clauses : 'a. st -> (st -> 'a) -> 'a list =
 fun s item ->
  if peek s <> INDENT then unexpected s "an indented block of clauses on the following lines";
  adv s;
  let rec go acc =
    let c = item s in
    match peek s with
    | NEWLINE ->
        adv s;
        go (c :: acc)
    | DEDENT ->
        adv s;
        List.rev (c :: acc)
    | _ -> unexpected s "end of line"
  in
  go []

(* ---- declarations ---------------------------------------------------- *)

let parse_variant s : string * sty list =
  match peek s with
  | CON c ->
      adv s;
      if peek s = SYM "(" then (c, paren_list s parse_type) else (c, [])
  | _ -> unexpected s "a constructor name"

let parse_data s : decl =
  adv s;
  let name =
    match peek s with
    | CON c ->
        adv s;
        c
    | _ -> unexpected s "a type name"
  in
  let params = if peek s = SYM "(" then paren_list s (fun s -> ident s "a type variable") else [] in
  expect_sym s "=";
  let ctors =
    if peek s = INDENT then (
      adv s;
      let rec line acc =
        if peek s = SYM "|" then adv s;
        let v = parse_variant s in
        let acc = v :: acc in
        match peek s with
        | SYM "|" ->
            adv s;
            line acc
        | NEWLINE ->
            adv s;
            line acc
        | DEDENT ->
            adv s;
            List.rev acc
        | _ -> unexpected s "`|`, a new line of variants, or the end of the block"
      in
      line [])
    else
      let rec go acc =
        let v = parse_variant s in
        if peek s = SYM "|" then (
          adv s;
          go (v :: acc))
        else List.rev (v :: acc)
      in
      go []
  in
  DData (name, params, ctors)

let parse_effect s : decl =
  adv s;
  let name = ident s "an effect name" in
  let ops =
    if peek s = INDENT then (
      adv s;
      let rec go acc =
        let o = ident s "an operation name" in
        let args = paren_list s parse_type in
        expect_sym s "->";
        let res = parse_type s in
        let acc = (o, args, res) :: acc in
        match peek s with
        | NEWLINE ->
            adv s;
            go acc
        | DEDENT ->
            adv s;
            List.rev acc
        | _ -> unexpected s "end of line"
      in
      go [])
    else []
  in
  DEffect (name, ops)

let parse_item s : decl =
  match peek s with
  | ID "data" -> parse_data s
  | ID "effect" -> parse_effect s
  | ID "let" ->
      adv s;
      let x = ident s "a name" in
      expect_sym s "=";
      DDef (false, x, parse_body s)
  | ID "fn" when (match (peek2 s, peek3 s) with ID _, SYM "(" -> true | _ -> false) ->
      adv s;
      let name, params, body = parse_fn_rest s in
      DDef (true, name, Lam (params, body))
  | _ -> DExpr (parse_expr s)

let program (src : string) : (pos * decl) list =
  let s = { toks = lex src; p = 0 } in
  let rec go acc =
    while peek s = NEWLINE do
      adv s
    done;
    match peek s with
    | EOF -> List.rev acc
    | INDENT -> fail s "unexpected indentation"
    | _ ->
        let pos = (cur s).pos in
        let d = parse_item s in
        (match peek s with
        | NEWLINE | EOF -> ()
        | _ -> unexpected s "end of line");
        go ((pos, d) :: acc)
  in
  go []
