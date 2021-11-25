(** Power function implementations for builtin integers *)

open Builtin
open Basic_arithmetics

(** Naive power function. Linear complexity
    @param x base
    @param n exponent
 *)
let pow x n = let rec inner = function
    | 0 -> if x = 0 then failwith "0 power 0 isn't defined" else 1
    | 1 -> x
    | n -> x * (inner (n-1))   
    in inner n;;

(** Fast integer exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
 *)
let power x n = if n mod 2 = 0 then 
    (power (x*x) (n/2) else x * power (x * x) (n/2 - 1);

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
    @param m modular base
 *)
let mod_power x n m = 0

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base
    @param n exponent
    @param p prime modular base
 *)
let prime_mod_power x n p = 0
