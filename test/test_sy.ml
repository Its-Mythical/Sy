open Sy

let failures = ref 0
let passed = ref 0

let report ok name detail =
  if ok then incr passed
  else (
    incr failures;
    Printf.printf "FAIL %s\n%s\n" name detail)

(* run a program, returning Ok output | Error message *)
let run ?(mode = Driver.Run) src =
  let buf = Buffer.create 64 in
  let s = Driver.make_session ~out:(Buffer.add_string buf) () in
  try
    Driver.run_source s ~mode src;
    Ok (Buffer.contents buf)
  with Driver.Sy_error (_, m) -> Error m

let contains hay needle =
  let n = String.length needle and h = String.length hay in
  let rec go i = i + n <= h && (String.sub hay i n = needle || go (i + 1)) in
  n = 0 || go 0

let expect_out name src expected =
  match run src with
  | Ok out -> report (out = expected) name (Printf.sprintf "  expected %S\n  got      %S" expected out)
  | Error m -> report false name ("  unexpected error: " ^ m)

let expect_error name src fragment =
  match run ~mode:Driver.Check src with
  | Ok out -> report false name ("  expected an error containing " ^ fragment ^ ", but it checked: " ^ out)
  | Error m -> report (contains m fragment) name (Printf.sprintf "  expected %S in: %s" fragment m)

let expect_type name src name_def expected =
  match run ~mode:Driver.Check src with
  | Error m -> report false name ("  unexpected error: " ^ m)
  | Ok out ->
      let line = Printf.sprintf "%s : %s" name_def expected in
      report (contains out line) name (Printf.sprintf "  expected line %S in:\n%s" line out)

let () =
  (* ---- evaluation ---- *)
  expect_out "arith" {|(println (int->str (+ 1 (* 2 3))))|} "7\n";
  expect_out "let and lambda" {|(println (int->str (let f (fn (x) (* x x)) (f 9))))|} "81\n";
  expect_out "letrec" {|(println (int->str (letrec f (fn (n) (if (<= n 1) 1 (* n (f (- n 1))))) (f 10))))|} "3628800\n";
  expect_out "value printing" {|(Cons 1 (Cons 2 Nil))|} "(Cons 1 (Cons 2 Nil))\n";
  expect_out "strings" {|(println (++ "a" (++ "b" "c")))|} "abc\n";
  expect_out "case on literals" {|(println (case 3 (1 "one") (3 "three") (_ "other")))|} "three\n";
  expect_out "unit is not echoed" {|(def x 1) x (println "hi")|} "1\nhi\n";

  (* ---- the effect system ---- *)
  expect_out "throw/catch"
    {|(println (case (catch (fn () (throw "x"))) ((Ok n) "ok") ((Err m) m)))|} "x\n";
  expect_out "resumption (tail-resumptive reader)"
    {|(effect ask (ask () Int))
      (def with-ask (fn (v thunk) (handle (thunk) (ask (k) (k v)))))
      (println (int->str (with-ask 21 (fn () (+ (ask) (ask))))))|}
    "42\n";
  expect_out "nested handlers of the same effect shadow"
    {|(effect ask (ask () Int))
      (def with-ask (fn (v thunk) (handle (thunk) (ask (k) (k v)))))
      (println (int->str (with-ask 1 (fn () (+ (ask) (with-ask 10 (fn () (ask))))))))|}
    "11\n";
  expect_out "unhandled ops of other effects pass through a handler"
    {|(effect ask (ask () Int))
      (def with-ask (fn (v thunk) (handle (thunk) (ask (k) (k v)))))
      (println (case (catch (fn () (with-ask 5 (fn () (do (ask) (throw "inner")))))) ((Ok n) "ok") ((Err m) m)))|}
    "inner\n";

  (* ---- types and effect rows ---- *)
  expect_type "pure function" "(def f (fn (x) (+ x 1)))" "f" "(Int) -> Int";
  expect_type "print is io" {|(def hi (fn () (println "x")))|} "hi" "() -> <io> Unit";
  expect_type "effects accumulate"
    {|(effect st (get () Int))
      (def g (fn () (do (println "x") (get))))|}
    "g" "() -> <io,st> Int";
  expect_type "polymorphic effect row"
    "(def twice (fn (f x) (f (f x))))" "twice" "(((a) -> <e1> a) a) -> <e1> a";
  expect_type "local state is masked (pure to the outside)"
    (In_channel.with_open_bin "../examples/state.sy" In_channel.input_all)
    "sum-to" "(Int) -> Int";
  expect_type "let-polymorphism" "(def id (fn (x) x)) (def a (id 1)) (def b (id \"s\"))" "b" "Str";

  (* ---- things that must be rejected ---- *)
  expect_error "unhandled effect at top level"
    {|(effect st (get () Int)) (get)|} "effect `st` is not available here";
  expect_error "effect leak through a function"
    {|(effect st (get () Int)) (def f (fn () (get))) (f)|} "effect `st` is not available here";
  expect_error "type mismatch" {|(+ 1 "two")|} "cannot unify";
  expect_error "arity" {|(def f (fn (a b) a)) (f 1)|} "expects 2 argument(s)";
  expect_error "unbound variable" {|(foo 1)|} "unbound variable `foo`";
  expect_error "non-exhaustive match"
    {|(def f (fn (xs) (case xs ((Cons x r) x))))|} "pattern `Nil` is not covered";
  expect_error "non-exhaustive nested"
    {|(def f (fn (p) (case p ((Pair True x) x) ((Pair False 0) 1))))|} "not covered";
  expect_error "partial handler"
    {|(effect st (get () Int) (put (Int) Unit))
      (def f (fn (t) (handle (t) (get (k) (k 0)))))|}
    "must handle all of its operations";
  expect_error "branches disagree" {|(def f (fn (b) (if b 1 "no")))|} "branch 2 of case";
  expect_error "infinite type" {|(def f (fn (x) (x x)))|} "infinite type";
  expect_error "syntax error" "(def f (fn (x) " "syntax error";
  expect_error "defrec needs a function" "(defrec x 1)" "must bind a function";
  expect_error "return clause type" 
    {|(effect e (op () Int)) (def f (fn (t) (handle (t) (op (k) "str") (return (x) (+ x 1)))))|} "handler clause";

  (* ---- check-then-run: a late type error means nothing runs ---- *)
  (let buf = Buffer.create 16 in
   let s = Driver.make_session ~out:(Buffer.add_string buf) () in
   (try Driver.run_source s ~mode:Driver.Run {|(println "side effect") (+ 1 "x")|}
    with Driver.Sy_error _ -> ());
   report (Buffer.contents buf = "") "no output before a later type error"
     (Printf.sprintf "  printed %S" (Buffer.contents buf)));

  (* ---- example programs: output must match the .expected files ---- *)
  let dir = if Array.length Sys.argv > 1 then Sys.argv.(1) else "../examples" in
  Array.iter
    (fun f ->
      if Filename.check_suffix f ".sy" then
        let base = Filename.chop_suffix f ".sy" in
        let src = In_channel.with_open_bin (Filename.concat dir f) In_channel.input_all in
        let expected =
          In_channel.with_open_bin (Filename.concat dir (base ^ ".expected")) In_channel.input_all
        in
        expect_out ("example " ^ base) src expected)
    (Sys.readdir dir);

  Printf.printf "%d passed, %d failed\n" !passed !failures;
  if !failures > 0 then exit 1
