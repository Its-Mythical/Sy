# SY surface syntax (draft 1)

The default-lane surface: indentation-significant, low-noise, and **desugared entirely into the seven core forms** while parsing (`src/surface.ml`). The uniform S-expression form (`.sx`) is always available and runs through the same checker and evaluator; `sy core FILE.sy` prints any surface program as its core form.

This is a *draft spec derived from a working parser*, not a frozen language. Items marked **(open)** are deliberate gaps.

## Lexical structure

- **Layout.** Indentation is spaces only (tabs are an error). A line indented more than the current block opens an `INDENT`; a line indented less closes blocks (`DEDENT`) and must line up exactly with an enclosing level. Each logical line ends with `NEWLINE`.
- **Suppressed layout.** Newlines and indentation are ignored inside `( … )`. A line also continues onto the next if it ends in an infix operator or `,`, or if the next line starts with `|>` and is indented further.
- **Comments** run from `#` to end of line.
- **Names.** `lower_snake` for values (`_` allowed), `UpperCamel` for types, constructors, and nothing else. No `-` in identifiers (so `int_to_str`, not `int->str`).
- **Literals.** Decimal integers, double-quoted strings with `\n \t \" \\`.
- **Keywords.** `fn let data effect match handle if then else return`.

## Grammar

```
program    := { item NEWLINE }
item       := 'fn' NAME '(' [names] ')' '=' body          -- recursive function
            | 'let' NAME '=' body                          -- definition
            | 'data' TYPE [ '(' names ')' ] '=' variants
            | 'effect' NAME [ INDENT op { NEWLINE op } DEDENT ]
            | expr                                         -- evaluated, value printed unless Unit

body       := expr | INDENT block DEDENT
block      := stmt { NEWLINE stmt }                        -- must end with an expression
stmt       := 'let' NAME '=' body
            | 'fn' NAME '(' [names] ')' '=' body           -- local recursive function
            | expr

expr       := expr BINOP expr | '-' expr | postfix
postfix    := atom { '(' [expr { ',' expr }] ')' }         -- call, repeatable: f(a)(b)
atom       := INT | STRING | NAME | CON | '(' ')' | '(' expr ')'
            | 'fn' NAME '->' body
            | 'fn' '(' [names] ')' '->' body
            | 'if' expr 'then' body [NEWLINE] 'else' body
            | 'match' expr INDENT arm { NEWLINE arm } DEDENT
            | 'handle' expr INDENT clause { NEWLINE clause } DEDENT

arm        := pat '->' body
pat        := '_' | NAME | INT | '-' INT | STRING | CON [ '(' pat { ',' pat } ')' ]
clause     := NAME '(' names ',' NAME ')' '->' body        -- op(args..., k) -> ...
            | 'return' '(' NAME ')' '->' body

variants   := variant { '|' variant } | INDENT ['|'] variant { ('|'|NEWLINE) variant } DEDENT
variant    := CON [ '(' type { ',' type } ')' ]
op         := NAME '(' [type {',' type}] ')' '->' type
type       := TYPE [ '(' type { ',' type } ')' ] | tvar
            | '(' [type {',' type}] ')' '->' [ '<' NAME {',' NAME} '>' ] type
```

### Operator precedence (low → high)

| level | operators | associativity |
|---|---|---|
| 1 | `\|>` | left |
| 2 | `\|\|` | left |
| 3 | `&&` | left |
| 4 | `== != < <= > >=` | left |
| 5 | `+ - ++` | left |
| 6 | `* / %` | left |
| 7 | unary `-` | prefix |
| 8 | call `f(…)` | postfix |

## Desugaring table

| Surface | Core |
|---|---|
| `fn f(x, y) = e` (top level) | `defrec f = Lam([x,y], e)` |
| `fn f(x, y) = e` (in a block) | `Let rec f = Lam([x,y], e) in …` |
| `let x = e` (top level) | `def x = e` |
| `let x = e` (in a block) | `Let x = e in <rest of block>` |
| expression statement `e` (not last) | `Let _ = e in <rest>` |
| `fn x -> e`, `fn (x, y) -> e`, `fn () -> e` | `Lam([…], e)` |
| `f(a, b)` | `App(f, [a, b])` |
| `Con(a, b)` / `Con` | `App(Var Con, [a, b])` / `Var Con` — constructors are ordinary values |
| `()` | `Var Unit` |
| `a + b`, `a == b`, `a ++ b`, … | `App(Var "+", [a, b])` — operators are ordinary functions |
| `-n` (literal) | `Lit (-n)` |
| `-e` | `App(Var "-", [Lit 0, e])` |
| `if c then a else b` | `Case c { True -> a ; False -> b }` |
| `a && b` | `Case a { True -> b ; False -> False }` (short-circuit) |
| `a \|\| b` | `Case a { True -> True ; False -> b }` (short-circuit) |
| `x \|> f(a)` | `App(f, [x, a])` — pipe inserts the left side as the **first** argument |
| `x \|> f` | `App(f, [x])` |
| `match e` + arms | `Case(e, arms)` |
| `handle e` + clauses | `Handle(e, { op(args,k) -> … ; ret(x) -> … })` |
| `effect E` + ops | effect declaration: each op is typed as `(args) -> <E\|ρ> result` |
| `data T(a) = …` | type declaration; each constructor generalised over `a` |

**Data-first convention.** Library functions take the collection first (`map(xs, f)`, `fold(xs, init, f)`), so `xs |> map(f) |> sum` reads left to right.

## Not in this draft (open)

- **Type annotations** (`x: T`, `-> T` on functions) — rejected with a clear error. The checker infers everything; annotations need a checking mode (design §08 bidirectional) and arrive with that.
- **Mutual recursion**, **string interpolation**, **tuples**, **operator sections** like `(+)`.
- **Control-lane syntax**: `own` parameters, `@unscoped`, `comptime`, `unsafe:` blocks from design §05.
- **Brace/indentation fork.** This draft is indentation-based, as the design states. See `DESIGN-NOTES.md`.
