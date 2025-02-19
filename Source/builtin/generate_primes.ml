(** Generating primes *)

open Builtin
open Basic_arithmetics

(** List composed of 2 and then odd integers starting at 3.
    @param n limit of list of odd integers, minimum value is 2.
 *)
let init_eratosthenes n =
  let rec loop = function x when x > n -> [] | x -> x :: loop (x + 2) in
  2 :: loop 3

(** Eratosthene sieve.
    @param n limit of list of primes, starting at 2.
*)
let eratosthenes n =
  let rec remove_div div = function
    | [] -> []
    | e :: t ->
        (if modulo e div = 0 && e <> div then 0 else e) :: remove_div div t
  in
  let rec loop list = function
    | [] -> list
    | 0 :: t -> loop list t
    | e :: t -> loop (remove_div e list) t
  in
  let sieve = init_eratosthenes n in
  let rec remove_zero = function
    | [] -> []
    | 0 :: t -> remove_zero t
    | x :: t -> x :: remove_zero t
  in
  remove_zero (loop sieve sieve)

(** Write a list into a file. Element seperator is newline.
    @param file path to write to.
 *)
let write_list li file = ()

(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime numbers up to which to build up a list of primes.
    @param file path to write to.
*)
let write_list_primes n file = ()

(** Read file safely ; catch End_of_file exception.
    @param in_c input channel.
 *)
let input_line_opt in_c = try Some (input_line in_c) with End_of_file -> None

(** Create a list out of reading a line per line channel.
    @param in_c input channel.
 *)
let create_list in_c =
  let rec _create_list in_c =
    match input_line_opt in_c with
    | Some line -> int_of_string line :: _create_list in_c
    | None -> []
  in
  _create_list in_c

(** Load list of primes into OCaml environment.
    @param file path to load from.
 *)
let read_list_primes file = []

(** Get biggest prime.
    @param l list of prime numbers.
 *)
let rec last_element l =
  match l with
  | [] -> failwith "You're list is empty. "
  | [ e ] -> e
  | h :: t -> last_element t

(** Get two biggest primes.
    @param l list of prime numbers.
 *)
let rec last_two l =
  match l with
  | [] | [ _ ] -> failwith "List has to have at least two prime numbers."
  | [ e; g ] -> (e, g)
  | h :: t -> last_two t

(** Finding couples of primes where second entry is twice the first
    plus 1.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let double_primes limit isprime =
  let rec loop = function
    | x when x > limit -> []
    | x ->
        if isprime x && isprime ((x * 2) + 1) then
          (x, (x * 2) + 1) :: loop (x + 1)
        else loop (x + 1)
  in
  loop 2

(** Finding twin primes.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let twin_primes limit isprime =
  let rec loop = function
    | x when x > limit -> []
    | x ->
        if isprime x && isprime (x + 2) then (x, x + 2) :: loop (x + 1)
        else loop (x + 1)
  in
  loop 2
