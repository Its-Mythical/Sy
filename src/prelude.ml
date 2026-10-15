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
  (fn (f xs)
    (case xs
      (Nil Nil)
      ((Cons x rest) (Cons (f x) (map f rest))))))

(defrec fold
  (fn (f acc xs)
    (case xs
      (Nil acc)
      ((Cons x rest) (fold f (f acc x) rest)))))

(defrec filter
  (fn (p xs)
    (case xs
      (Nil Nil)
      ((Cons x rest) (if (p x) (Cons x (filter p rest)) (filter p rest))))))

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
  (fn (xs) (fold (fn (acc x) (Cons x acc)) Nil xs)))

(defrec range
  (fn (lo hi)
    (if (>= lo hi) Nil (Cons lo (range (+ lo 1) hi)))))

(def sum (fn (xs) (fold + 0 xs)))

(defrec show-list
  (fn (xs)
    (case xs
      (Nil "")
      ((Cons x Nil) (int->str x))
      ((Cons x rest) (++ (int->str x) (++ " " (show-list rest)))))))

;; Run a computation that may `throw`, turning the effect into a value.
(def catch
  (fn (thunk)
    (handle (thunk)
      (throw (msg k) (Err msg))
      (return (x) (Ok x)))))
|}
