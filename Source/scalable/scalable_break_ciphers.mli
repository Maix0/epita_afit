(** Factoring bitarrays into primes *)

val break : int list * 'a -> int list * int list
(** Factors product of two prime bitarrays.
    @param key is public key of an RSA cryptosystem.
 *)
