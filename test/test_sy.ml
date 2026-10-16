open Sy

let failures = ref 0
let passed = ref 0

let report ok name detail =
  if ok then incr passed
  else (
    incr failures;
    Printf.printf "FAIL %s\n%s\n" name detail)

(* run a program, returning Ok output | Error message *)
let run ?(mode = Driver.Run) ?(syntax = Driver.Sexpr) src =
  let buf = Buffer.create 64 in
  let s = Driver.make_session ~out:(Buffer.add_string buf) () in
  try
    Driver.run_source ~syntax s ~mode src;
    Ok (Buffer.contents buf)
  with Driver.Sy_error (_, m) -> Error m

let contains hay needle =
  let n = String.length needle and h = String.length hay in
  let rec go i = i + n <= h && (String.sub hay i n = needle || go (i + 1)) in
  n = 0 || go 0

let expect_out ?(syntax = Driver.Sexpr) name src expected =
  match run ~syntax src with
  | Ok out -> report (out = expected) name (Printf.sprintf "  expected %S\n  got      %S" expected out)
  | Error m -> report false name ("  unexpected error: " ^ m)

let expect_error ?(syntax = Driver.Sexpr) name src fragment =
  match run ~mode:Driver.Check ~syntax src with
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
    (In_channel.with_open_bin "../examples/state.sx" In_channel.input_all)
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


  (* ---- the indentation surface ---- *)
  let sy = Driver.Surface in
  let out n src e = expect_out ~syntax:sy ("surface: " ^ n) src e in
  let bad n src frag = expect_error ~syntax:sy ("surface: " ^ n) src frag in
  out "precedence" "println(int_to_str(1 + 2 * 3 - 4))" "3\n";
  out "parens" "println(int_to_str((1 + 2) * 3))" "9\n";
  out "left associativity" "println(int_to_str(10 - 3 - 2))" "5\n";
  out "unary minus" "println(int_to_str(-3 + 5))" "2\n";
  out "negative literal pattern"
    "fn f(n) =\n  match n\n    -1 -> \"minus one\"\n    _ -> \"other\"\nprintln(f(-1))" "minus one\n";
  out "comparison and if" {|println(if 2 < 3 then "yes" else "no")|} "yes\n";
  out "&& short-circuits"
    "println(if False && (1 / 0 == 1) then \"a\" else \"b\")" "b\n";
  out "|| short-circuits"
    "println(if True || (1 / 0 == 1) then \"a\" else \"b\")" "a\n";
  out "string concat" {|println("a" ++ "b" ++ "c")|} "abc\n";
  out "unit literal" "let u = ()\nprintln(\"ok\")" "ok\n";
  out "comments and blank lines" "# leading comment\n\nprintln(\"x\")   # trailing\n\n\n" "x\n";
  out "block with let and sequencing"
    "fn f(x) =\n  let y = x + 1\n  let z = y * 2\n  z\nprintln(int_to_str(f(4)))" "10\n";
  out "local recursive fn"
    "fn fact(n) =\n  fn go(i, acc) = if i > n then acc else go(i + 1, acc * i)\n  go(1, 1)\nprintln(int_to_str(fact(5)))"
    "120\n";
  out "if/else on separate lines"
    "fn sign(n) =\n  if n < 0 then \"neg\"\n  else if n == 0 then \"zero\"\n  else \"pos\"\nprintln(sign(-2))\nprintln(sign(0))\nprintln(sign(9))"
    "neg\nzero\npos\n";
  out "if with block branches"
    "fn f(b) =\n  if b then\n    println(\"t\")\n    1\n  else\n    println(\"f\")\n    2\nprintln(int_to_str(f(True)))"
    "t\n1\n";
  out "match with block arms"
    "fn f(x) =\n  match x\n    Some(n) ->\n      let m = n * 2\n      m + 1\n    None -> 0\nprintln(int_to_str(f(Some(4))))"
    "9\n";
  out "match literals and wildcard"
    "fn f(n) =\n  match n\n    0 -> \"zero\"\n    1 -> \"one\"\n    _ -> \"many\"\nprintln(f(1))\nprintln(f(7))"
    "one\nmany\n";
  out "pipe inserts first argument"
    "println(show_list(range(1, 5) |> map(fn x -> x * x) |> filter(fn x -> x > 3)))" "4 9 16\n";
  out "leading pipe on continuation lines"
    "let r = range(1, 5)\n  |> map(fn x -> x + 1)\n  |> sum\nprintln(int_to_str(r))" "14\n";
  out "line ending in operator continues"
    "let r = 1 +\n  2 +\n  3\nprintln(int_to_str(r))" "6\n";
  out "newlines inside parentheses are ignored"
    "println(int_to_str(\n  1 + 2\n))" "3\n";
  out "lambda forms"
    "let f = fn x -> x + 1\nlet g = fn (a, b) -> a * b\nlet h = fn () -> 7\nprintln(int_to_str(f(1) + g(2, 3) + h()))" "15\n";
  out "multi-line data declaration"
    "data Color =\n  | Red\n  | Green\n  | Blue(Int)\nfn name(c) =\n  match c\n    Red -> \"r\"\n    Green -> \"g\"\n    Blue(n) -> \"b\" ++ int_to_str(n)\nprintln(name(Blue(3)))"
    "b3\n";
  out "parameterised data"
    "data Tree(a) = Leaf | Node(Tree(a), a, Tree(a))\nfn size(t) =\n  match t\n    Leaf -> 0\n    Node(l, _, r) -> size(l) + 1 + size(r)\nprintln(int_to_str(size(Node(Node(Leaf, 1, Leaf), 2, Leaf))))"
    "2\n";
  out "effect with no operations" "effect nothing\nprintln(\"ok\")" "ok\n";

  (* ---- surface errors: position and message quality ---- *)
  bad "tabs rejected" "fn f(x) =\n\tx" "tabs are not allowed";
  bad "bad dedent" "fn f(x) =\n    let a = 1\n  a" "inconsistent indentation";
  bad "unexpected indent" "  println(\"x\")" "unexpected indentation";
  bad "block must end in expression" "fn f(x) =\n  let y = x" "must end with an expression";
  bad "annotations rejected" "fn f(x: Int) = x" "type annotations are not supported yet";
  bad "return annotation rejected" "fn f(x) -> Int = x" "return-type annotations are not supported yet";
  bad "missing else" "let x = if True then 1" "expected `else`";
  bad "unclosed paren" "println(\"x\"" "unclosed";
  bad "let in expression position" "let x = 1 + let y = 2" "only allowed as a statement";
  bad "type error through the surface" "println(1 + \"a\")" "cannot unify";
  bad "unhandled effect through the surface"
    "effect st\n  get() -> Int\nget()" "effect `st` is not available here";
  bad "non-exhaustive through the surface"
    "fn f(x) =\n  match x\n    Some(n) -> n" "pattern `None` is not covered";
  bad "partial handler through the surface"
    "effect st\n  get() -> Int\n  put(Int) -> Unit\nfn f(t) =\n  handle t()\n    get(k) -> k(0)"
    "must handle all of its operations";
  (let msg = (match run ~mode:Driver.Check ~syntax:sy "fn f(x) =\n  x +\n\n  )" with Error m -> m | Ok _ -> "") in
   report (msg <> "") "surface: stray token reports an error" "  expected a syntax error");
  (* position of a type error is the start of the declaration *)
  (let buf = Buffer.create 8 in
   let s = Driver.make_session ~out:(Buffer.add_string buf) () in
   match Driver.run_source ~syntax:sy s ~mode:Driver.Check "println(\"a\")\n\nfn f() = 1 + \"x\"\n" with
   | exception Driver.Sy_error (p, _) -> report (p.line = 3 && p.col = 1) "surface: error position" (Printf.sprintf "  got line %d col %d" p.line p.col)
   | () -> report false "surface: error position" "  expected an error");

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
      let syntax =
        if Filename.check_suffix f ".sy" then Some Driver.Surface
        else if Filename.check_suffix f ".sx" then Some Driver.Sexpr
        else None
      in
      match syntax with
      | None -> ()
      | Some syntax ->
          let base = Filename.remove_extension f in
          let src = In_channel.with_open_bin (Filename.concat dir f) In_channel.input_all in
          let expected =
            In_channel.with_open_bin (Filename.concat dir (base ^ ".expected")) In_channel.input_all
          in
          expect_out ~syntax ("example " ^ f) src expected)
    (Sys.readdir dir);

  Printf.printf "%d passed, %d failed\n" !passed !failures;
  if !failures > 0 then exit 1
