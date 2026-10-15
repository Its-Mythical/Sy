# Sy

**SY** is a strict, immutable, effect-typed general-purpose language designed as a *synthesis* of the best proven ideas from earlier languages, held together by one rule: *coherence before coverage*. The full design is in [`sy-language.html`](sy-language.html) (open it in a browser).

This repository holds the **reference implementation**, written in OCaml. It is **Milestone 1**: the semantic core of the language, end to end.

## What works today

| Design section | In this repo |
|---|---|
| §03 The core: seven forms | `src/ast.ml` — `Var · Lit · Lam · App · Let · Case · Handle`; everything else desugars to these (`sy core FILE` shows the lowering) |
| §11 Effect-spine | `src/types.ml`, `src/infer.ml` — Hindley–Milner with **row-polymorphic effect inference**. Latent effect rows on function types, `Handle` discharges effects. I/O, errors, and user effects all use the one mechanism |
| §12 Local effect masking | A function that uses `state` internally and handles it before returning is inferred **pure** (`sum-to : (Int) -> Int` in `examples/state.sy`) |
| §13 Handlers, incl. multi-shot | `src/eval.ml` — deep handlers whose resumption is an ordinary closure; `examples/nondeterminism.sy` resumes twice |
| §01 Strict core | Call-by-value evaluator; constructors are first-class functions |
| Exhaustive matching | Maranget-style check; errors name an uncovered pattern |
| Diagnostics | Type errors carry file:line:col and the enclosing definition |

`sy run` type- and effect-checks the **whole program first**; if anything fails, nothing runs.

## Build and run

Requires OCaml ≥ 4.14 and dune ≥ 3.0.

```sh
dune build
dune test                         # 35 checks: semantics, types, rejections, examples
dune exec -- sy run   examples/state.sy
dune exec -- sy check examples/state.sy   # print inferred types
dune exec -- sy core  examples/state.sy   # print the seven-form lowering
```

## A taste

The surface in Milestone 1 is the **uniform core form** (S-expressions) — the "always reachable" view from Axis 1. The human-facing surface syntax is the next milestone (see below).

```lisp
(effect state (get () Int) (put (Int) Unit))

(def run-state
  (fn (init thunk)
    ((handle (thunk)
       (get (k)    (fn (s) ((k s) s)))
       (put (s2 k) (fn (s) ((k Unit) s2)))
       (return (x) (fn (s) x)))
     init)))

(def sum-to (fn (n) (run-state 0 (fn () (count-down n)))))
;; sum-to : (Int) -> Int      <- pure, although it used `state` inside
```

Calling an effectful function where its effect isn't handled is a type error:

```
prog.sy:5:1: type error: calling `thunk`: effect `st` is not available here
(the enclosing context only allows <io>)
```

## Layout

```
src/ast.ml       the seven core forms
src/sexp.ml      reader for the uniform core form
src/parse.ml     S-expr -> core AST (+ `if`, `do` sugar)
src/types.ml     types, effect rows, unification, generalization
src/infer.ml     inference, handler typing, exhaustiveness
src/value.ml     runtime values (free-monad style results)
src/eval.ml      evaluator with deep, multi-shot handlers
src/prims.ml     built-in primitives (typed in SY's own syntax)
src/prelude.ml   the prelude — ordinary SY source, checked like user code
src/driver.ml    check-then-run pipeline
bin/main.ml      `sy` CLI
examples/        runnable programs with `.expected` output
test/            dune test suite
docs/            design notes and roadmap
```

## Not yet (honest list)

This is a semantics-first interpreter. It deliberately does **not** yet have: a human-facing surface syntax, explicit type/effect annotations, mutual recursion, parameterised effects, the implicit handler boundary of §13, scoped arenas, Perceus reference counting, handler specialization, `comptime`, macros, modules, or FFI. See [`docs/DESIGN-NOTES.md`](docs/DESIGN-NOTES.md) for decisions and the roadmap.
