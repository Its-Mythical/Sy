# Design notes and roadmap

## Decisions made in Milestone 1

**Implementation language: OCaml for the front/middle end.** Sum types and pattern matching are first-class, which is exactly the shape of AST/IR work. Its GC is irrelevant for the compiler itself (only the runtime SY *emits* must be GC-free). The runtime/backend (Perceus RC, arenas, FFI, native codegen) is expected to be Rust or Zig later; the long-term endgame is self-hosting.

**Two readers, one core.** Milestone 1 used S-expressions (`.sx`) — the "uniform core form always reachable" of Axis 1 — so the semantics could be built and tested first. Milestone 2 added the human-facing indentation surface (`.sy`, `src/surface.ml`). Both desugar into the same seven-form AST and share the checker and evaluator; every example exists in both forms with one `.expected` file, which is the equivalence test. `sy core FILE.sy` shows any surface program lowered. Spec: [`SURFACE.md`](SURFACE.md).

**Indentation vs. braces — still an open fork, and this repo currently sides with the design text.** The design (§05, §13) is indentation-significant, so `src/surface.ml` is too. The revised §15 records that an earlier **Python prototype** chose braces; that prototype is not in this repository, so it was not consulted. Because the surface is a hand-written parser that desugars directly, swapping the layout rules for braces would touch only the lexer's layout pass and a few parser entry points — the core, checker and evaluator are unaffected. Decide deliberately; don't let it drift. (If the Python prototype is added to the repo, its grammar should be reconciled against `SURFACE.md`.)

**Data-first library convention.** `map(xs, f)`, `fold(xs, init, f)`, `filter(xs, p)`: the collection comes first, so `xs |> map(f) |> sum` reads left to right (matching the design's own `urls |> map(...) |> join`). `|>` inserts its left side as the first argument.

**Annotations are rejected, not ignored.** `x: T` and `-> T` produce an explicit "not supported yet" error rather than being silently dropped; they arrive with the bidirectional checking mode (§08).

**Effects are Koka-style scoped rows.** A row is a list of labels ending in empty or a variable. Function types carry a latent row; a call unifies it with the ambient row; `Lam` opens a fresh row for its body; `Handle e` types `e` under `<labels | ambient>`. This gives purity as "empty residual row" with no annotations.

**Value restriction.** Only syntactic values (lambdas, literals, variables, constructor applications of values) are generalized.

**Handlers must be total per effect.** A handler for an effect must handle all of its operations (partial handlers are rejected). Simpler to reason about; can be relaxed later.

**Check-then-run.** The whole program is type- and effect-checked before anything executes.

## Design doc status

`sy-language.html` was updated (by the author) with a revised §15: the surface "exists, but isn't specified", and a new "Which language is the compiler written in?" entry proposing OCaml (front/middle-end) + Rust or Zig (runtime/backend) with self-hosting as the target. This repo follows that proposal. With Milestone 2 the first half of the surface gap is closed: a formal grammar and desugaring table now exist (`SURFACE.md`). What §15 still says that remains true: the brace/indentation fork is undecided, and the spec is derived from a parser rather than the other way around.

## Known limitations of Milestone 1

- No mutual recursion (`letrec`/`defrec` bind one function); definitions must precede use.
- No tuples, string interpolation, or operator sections; mutual recursion is still absent in both syntaxes.
- No explicit type or effect annotations, hence no higher-rank types or polymorphic recursion (§08 says these need signatures).
- Effects are not parameterised (no `state<a>`); op signatures may use free type variables (`throw : (Str) -> a`).
- Handlers are deep and general. There is **no** specialization of tail-resumptive / single-shot handlers (§13 Invariant 2) — that claim remains "taken on faith until built".
- The evaluator uses the host stack; very deep non-tail recursion can overflow (tested to ~100k frames).
- Strings are bytes; Int is the host's 63-bit int.

## Roadmap

1. ~~**M2 — Surface syntax.**~~ Done: indentation surface, grammar, desugaring table (`SURFACE.md`). Still to do under this heading: source locations on the AST for finer diagnostics, a formatter, and a decision on indentation vs. braces.
2. **M3 — Annotations and the §13 invariants in the checker.** Bidirectional checking with optional signatures (which also unlocks higher-rank types and effect-signature pinning); the implicit handler boundary for localizable effects (and the obligation that they be tail-resumptive/single-shot); phantom scope tokens (runST-style) so local mutable state cannot escape; acyclicity as a type attribute (the open question in §15).
3. **M4 — A real IR and handler specialization.** Lower the core to an IR; compile tail-resumptive handlers to direct code, single-shot to stack unwinding/state machines, multi-shot to frame cloning. Benchmark against the §15 performance claim.
4. **M5 — Perceus.** Precise ownership/last-use analysis, reference counting with reuse; scoped arenas for the cycles strictness can't exclude.
5. **M6 — Backend + runtime** (Rust or Zig; LLVM), then the FFI boundary (§14) and ambient-effect tagging.
6. **M7 — Extensibility.** Hygienic macros over the core, then `comptime`, then `#lang`-style layers (Axes 0 and 6).
7. **Endgame — self-hosting.**
