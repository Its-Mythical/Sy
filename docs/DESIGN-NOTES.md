# Design notes and roadmap

## Decisions made in Milestone 1

**Implementation language: OCaml for the front/middle end.** Sum types and pattern matching are first-class, which is exactly the shape of AST/IR work. Its GC is irrelevant for the compiler itself (only the runtime SY *emits* must be GC-free). The runtime/backend (Perceus RC, arenas, FFI, native codegen) is expected to be Rust or Zig later; the long-term endgame is self-hosting.

**Surface = the uniform core form, for now.** The design (§15, "Axis 1") says the surface grammar and surface-to-core desugaring table are unwritten. Rather than pick a surface by accident, Milestone 1 uses S-expressions — the "uniform core form always reachable" of Axis 1 — so the semantics could be built and tested first. `if` and `do` are the only sugar, and `sy core` shows their lowering.

**Open fork, deliberately not decided here: indentation vs. braces.** The design's own examples (§05, §13) are indentation-significant. A brace-based surface was mentioned in discussion, but **no such implementation exists in this repository**, and nothing here depends on either choice. This has to be settled when the surface is written (Milestone 2).

**Effects are Koka-style scoped rows.** A row is a list of labels ending in empty or a variable. Function types carry a latent row; a call unifies it with the ambient row; `Lam` opens a fresh row for its body; `Handle e` types `e` under `<labels | ambient>`. This gives purity as "empty residual row" with no annotations.

**Value restriction.** Only syntactic values (lambdas, literals, variables, constructor applications of values) are generalized.

**Handlers must be total per effect.** A handler for an effect must handle all of its operations (partial handlers are rejected). Simpler to reason about; can be relaxed later.

**Check-then-run.** The whole program is type- and effect-checked before anything executes.

## Suggested edit to the design doc (§15, Axis 1)

Replace "the surface syntax is unspecified" with: *a first surface exists (the S-expression core form, in the implementation); the human-facing grammar, the indentation rules, and the formal surface→core desugaring table remain to be written, and the indentation-vs-braces choice is unresolved.* This has **not** been applied to `sy-language.html`; say the word and it will be.

## Known limitations of Milestone 1

- No mutual recursion (`letrec`/`defrec` bind one function); definitions must precede use.
- No explicit type or effect annotations, hence no higher-rank types or polymorphic recursion (§08 says these need signatures).
- Effects are not parameterised (no `state<a>`); op signatures may use free type variables (`throw : (Str) -> a`).
- Handlers are deep and general. There is **no** specialization of tail-resumptive / single-shot handlers (§13 Invariant 2) — that claim remains "taken on faith until built".
- The evaluator uses the host stack; very deep non-tail recursion can overflow (tested to ~100k frames).
- Strings are bytes; Int is the host's 63-bit int.

## Roadmap

1. **M2 — Surface syntax.** Decide indentation vs. braces; write the grammar and the surface→core desugaring table; layout-aware lexer; keep the S-expr form as the always-reachable view. Add source locations to the AST for better diagnostics.
2. **M3 — The §13 invariants in the checker.** Implicit handler boundary for localizable effects (and the obligation that they be tail-resumptive/single-shot); phantom scope tokens (runST-style) so local mutable state cannot escape; acyclicity as a type attribute (the open question in §15).
3. **M4 — A real IR and handler specialization.** Lower the core to an IR; compile tail-resumptive handlers to direct code, single-shot to stack unwinding/state machines, multi-shot to frame cloning. Benchmark against the §15 performance claim.
4. **M5 — Perceus.** Precise ownership/last-use analysis, reference counting with reuse; scoped arenas for the cycles strictness can't exclude.
5. **M6 — Backend + runtime** (Rust or Zig; LLVM), then the FFI boundary (§14) and ambient-effect tagging.
6. **M7 — Extensibility.** Hygienic macros over the core, then `comptime`, then `#lang`-style layers (Axes 0 and 6).
7. **Endgame — self-hosting.**
