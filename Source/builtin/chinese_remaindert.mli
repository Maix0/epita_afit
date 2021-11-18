(** Chinese remainder theorem *)

val crt_image : int -> int list -> int list
(** Image of the Chinese Remainder map
    @param x positive integer of which you take image
    @param l list of pairwise relatively prime positive integers.
 *)

val crt_solver : int -> int list -> int list -> int
(** Inverse image of Chinese Remainder map
    @param m a positive integer
    @param l list of pairwise relatively prime factors of m
    @param y list of remainders modulo pairwise relatively prime factors of m
 *)
