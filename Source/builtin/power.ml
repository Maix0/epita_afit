(** Power function implementations for builtin integers *)

open Builtin
open Basic_arithmetics

(** Naive power function. Linear complexity
    @param x base
    @param n exponent
 *)
let pow x n =
  let rec inner = function
    | 0 -> if x = 0 then failwith "0 power 0 isn't defined" else 1
    | 1 -> x
    | n -> x * inner (n - 1)
  in
  inner n

(** Fast integer exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
 *)
let power x n =
  let rec inner a b =
    (*Printf.printf "a=%d|b=%d" a b;*)
    if b = 0 then if a = 0 then failwith "0 power 0 isn't defined" else 1
    else if b = 1 then a
    else if b = 2 then a * a
    else if b mod 2 = 0 then inner (inner a (quot b 2)) 2
    else a * inner (inner a (quot (b - 1) 2)) 2
  in
  inner x n

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
    @param m modular base
 *)
let mod_power x n m =
  let rec inner a b c =
    if a = 0 then 0
    else if b = 0 then 1
    else if b mod 2 = 0 then
      let y = inner a (quot b 2) c in
      let y = y * y mod c in
      (y + c) mod c
    else
      let y = a mod c in
      let y = y * (inner a (b - 1) c mod c) mod c in
      (y + c) mod c
  in
  inner x n m

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base
    @param n exponent
    @param p prime modular base
 *)
let prime_mod_power x n p = 0
