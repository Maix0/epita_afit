(** Basic arithmetics with builtin integers *)

open Builtin

(** Greater common (positive) divisor of two non-zero integers.
    @param a non-zero integer
    @param b non-zero integer
*)
let rec gcd a b = abs (if b = 0 then a else gcd b (modulo a b))

(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param a non-zero integer
    @param b non-zero integer.
*)
let bezout a b =
  let rec inner a b (p1, p2) (c1, c2) g =
    if b = 0 then (p1, p2, g)
    else
      let q, r = div a b in
      let x = p1 - (c1 * q) and y = p2 - (c2 * q) in
      inner b r (c1, c2) (x, y) g
  in

  inner a b (1, 0) (0, 1) (gcd a b)
