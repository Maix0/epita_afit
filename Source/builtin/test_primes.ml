(** Testing for primality *)

open Builtin
open Basic_arithmetics
open Power

(** Deterministic primality test *)

let is_prime n =
  (*Samelessly stolen from the seminar's algo correction *)
  if n < 2 then invalid_arg "prime: not defined for integers < 2"
  else
    let rec prime d = d * d > n || (n mod d <> 0 && prime (d + 2)) in
    n = 2 || (n mod 2 <> 0 && prime 3)

(** Primality test based on small Fermat theorem
    @param p tested integer
    @param testSeq sequence of integers against which to test
*)
let is_pseudo_prime p test_seq = 
