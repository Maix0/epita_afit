(** Testing for primality *)

val is_prime : int list -> bool
(** Deterministic primality test
*)

val is_pseudo_prime : int list -> int list list -> bool
(** Pseudo-primality test based on Fermat's Little Theorem
    @param p tested bitarray
    @param testSeq sequence of bitarrays againt which to test
 *)
