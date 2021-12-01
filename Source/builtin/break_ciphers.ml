(** Factoring Builtin int primes *)

open Builtin
open Basic_arithmetics

(** Factors product of two primes.
    @param key is public key of an RSA cryptosystem.
 *)
let break key =
  let n, e = key in
  let rec loop = function
    | 1 -> failwith "Unable to factorize"
    | x -> if modulo n x = 0 then x else loop (x - 2)
  in
  let q = loop (quot (n - 2) 2 + 1) in
  let p = quot n q in
  (p, q)
