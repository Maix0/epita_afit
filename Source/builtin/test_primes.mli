(** Testing for primality *)

val is_prime : int -> bool
(** Deterministic primality test
    @param n integer bigger or equal to 2.
*)

val is_pseudo_prime : int -> int list -> bool
(** Primality test based on small Fermat theorem
    @param p tested integer
    @param testSeq sequence of integers against which to test
 *)
