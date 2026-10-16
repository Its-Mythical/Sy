(* The prelude is ordinary SY source, type-checked and run by the same
   pipeline as user code — so it doubles as a self-test of the checker. *)

let source =
  {|
(data Unit (Unit))
(data Bool (False) (True))
(data (List a) (Nil) (Cons a (List a)))
(data (Option a) (None) (Some a))
(data (Pair a b) (Pair a b))
(data (Result a e) (Ok a) (Err e))

(effect io)
(effect exn (throw (Str) a))

(def not (fn (b) (if b False True)))
(def and (fn (a b) (if a b False)))
(def or  (fn (a b) (if a True b)))

(defrec map
  (fn (xs f)
    (case xs
      (Nil Nil)
      ((Cons x rest) (Cons (f x) (map rest f))))))

(defrec fold
  (fn (xs acc f)
    (case xs
      (Nil acc)
      ((Cons x rest) (fold rest (f acc x) f)))))

(defrec filter
  (fn (xs p)
    (case xs
      (Nil Nil)
      ((Cons x rest) (if (p x) (Cons x (filter rest p)) (filter rest p))))))

(defrec append
  (fn (xs ys)
    (case xs
      (Nil ys)
      ((Cons x rest) (Cons x (append rest ys))))))

(defrec length
  (fn (xs)
    (case xs
      (Nil 0)
      ((Cons x rest) (+ 1 (length rest))))))

(def reverse
  (fn (xs) (fold xs Nil (fn (acc x) (Cons x acc)))))

(defrec range
  (fn (lo hi)
    (if (>= lo hi) Nil (Cons lo (range (+ lo 1) hi)))))

(def sum (fn (xs) (fold xs 0 +)))

(defrec show_list
  (fn (xs)
    (case xs
      (Nil "")
      ((Cons x Nil) (int->str x))
      ((Cons x rest) (++ (int->str x) (++ " " (show_list rest)))))))

;; Surface-friendly names: the indentation syntax has no `-`/`>`/`=` in identifiers.
(def int_to_str int->str)
(def str_len str-len)
(def str_eq str=)

;; Run a computation that may `throw`, turning the effect into a value.
(def catch
  (fn (thunk)
    (handle (thunk)
      (throw (msg k) (Err msg))
      (return (x) (Ok x)))))
|}
