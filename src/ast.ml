(* SY core AST — the seven forms of §03.

     Var · Lit · Lam · App · Let · Case · Handle

   Everything the surface offers (if, do, constructors, performing an effect)
   lowers to these.  Type and effect abstraction/instantiation (the [tys][effs]
   parameters of Lam/App in the design) are inferred in Milestone 1 and so have
   no syntax here yet. *)

type lit =
  | LInt of int
  | LStr of string

type pat =
  | PWild
  | PVar of string
  | PLit of lit
  | PCon of string * pat list

type expr =
  | Var of string
  | Lit of lit
  | Lam of string list * expr
  | App of expr * expr list
  | Let of bool * string * expr * expr (* rec?, name, rhs, body *)
  | Case of expr * (pat * expr) list
  | Handle of expr * handler

and handler = {
  ops : (string * string list * string * expr) list;
      (* operation, argument names, resumption name, clause body *)
  ret : (string * expr) option; (* return clause: ret(x) -> r *)
}

(* Surface type syntax, used only in declarations. *)
type sty =
  | STVar of string
  | STCon of string * sty list
  | STFun of sty list * string list * sty (* args, effect labels, result *)

type decl =
  | DData of string * string list * (string * sty list) list
  | DEffect of string * (string * sty list * sty) list
  | DDef of bool * string * expr (* recursive?, name, rhs *)
  | DExpr of expr
