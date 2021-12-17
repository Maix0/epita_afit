(** A naive implementation of big integers

This module aims at creating a set of big integers naively. Such data
types will be subsequently called bitarrays. A bitarray is a list of
zeros and ones ; first integer representing the sign bit. In this
contexte zero is reprensented by the empty list []. The list is to
be read from left to right ; this is the opposite convention to the
one you usually write binary decompositions with. After the sign bit
the first encountered bit is the coefficient in front of two to
the power zero. This convention has been chosen to ease writing
down code.

 *)

(** Creates a bitarray from a built-in integer.
    @param x built-in integer.
*)
let from_int x =
  let num = if x < 0 then -x else x in
  if num = 0 then []
  else
    let rec inner = function
      | 0 -> [ (if x > 0 then 0 else 1) ]
      | n -> (n land 1) :: inner (n lsr 1)
    in
    let reverse list =
      let rec rev accu = function [] -> accu | e :: l -> rev (e :: accu) l in
      rev [] list
    in
    reverse (inner num)

(** Transforms bitarray of built-in size to built-in integer.
    UNSAFE: possible integer overflow.
    @param bA bitarray object.
 *)
let to_int bA =
  let rec build index = function
    | [] -> 0
    | e :: t -> (e lsl index) lor build (index + 1) t
  in
  match bA with
  | [] -> 0
  | bit :: tail -> (if bit = 1 then -1 else 1) * build 0 tail

(** Prints bitarray as binary number on standard output.
    @param bA a bitarray.
  *)
let print_b bA = ()

(** Toplevel directive to use print_b as bitarray printer.
    CAREFUL: print_b is then list int printer.
    UNCOMMENT FOR TOPLEVEL USE.
*)
(* #install_printer print_b *)

(** Internal comparisons on bitarrays and naturals. Naturals in this
    context are understood as bitarrays missing a bit sign and thus
    assumed to be non-negative.
*)

(** Comparing naturals. Output is 1 if first argument is bigger than
    second -1 otherwise.
    @param nA A natural, a bitarray having no sign bit.
           Assumed non-negative.
    @param nB A natural.
 *)
let rec compare_n nA nB =
  match (nA, nB) with
  | [], [] -> 0
  | [], _ :: _ -> -1
  | _ :: _, [] -> 1
  | e1 :: t1, e2 :: t2 ->
      if t1 = t2 then if e1 = e2 then 0 else if e1 > e2 then 1 else -1
      else compare_n t1 t2

(** Bigger inorder comparison operator on naturals. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let ( >>! ) nA nB = compare_n nA nB = 1

(** Smaller inorder comparison operator on naturals. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let ( <<! ) nA nB = compare_n nA nB = -1

(** Bigger or equal inorder comparison operator on naturals. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let ( >=! ) nA nB =
  let res = compare_n nA nB in
  res = 1 || res = 0

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let ( <=! ) nA nB =
  let res = compare_n nA nB in
  res = -1 || res = 0

(** Comparing two bitarrays. Output is 1 if first argument is bigger
    than second -1 otherwise.
    @param bA A bitarray.
    @param bB A bitarray.
*)
let compare_b bA bB =
  match (bA, bB) with
  | [], [] -> 0
  | 0 :: _, 1 :: _ | 1 :: _, [] -> 1
  | 1 :: _, 0 :: _ | [], 1 :: _ -> -1
  | h1 :: t1, h2 :: t2 -> compare_n t1 t2 * if h1 = 1 then 1 else -1
  | _, _ -> 0

(** Bigger inorder comparison operator on bitarrays. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let ( >> ) bA bB = compare_b bA bB = 1

(** Smaller inorder comparison operator on bitarrays. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let ( << ) bA bB = compare_b bA bB = -1

(** Bigger or equal inorder comparison operator on bitarrays. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let ( >>= ) bA bB =
  let res = compare_b bA bB in
  res = 1 || res = 0

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let ( <<= ) bA bB =
  let res = compare_b bA bB in
  res = -1 || res = 0

(** Sign of a bitarray.
    @param bA Bitarray.
*)
let sign_b bA = match bA with [] -> 0 | s :: _ -> if s = 1 then -1 else 1

(** Absolute value of bitarray.
    @param bA Bitarray.
*)
let abs_b bA = match bA with [] -> [] | _ :: t -> 0 :: t

(** Quotient of integers smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _quot_t a = if a < 2 then 0 else 1

(** Modulo of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _mod_t a = if a = 1 || a = 3 then 1 else 0

(** Division of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _div_t a = (_quot_t a, _mod_t a)

(** Addition of two naturals.
    @param nA Natural.
    @param nB Natural.
*)
let add_n nA nB =
  let rec adder carry = function
    | [], [] -> if carry = 1 then [ 1 ] else []
    | e :: t, [] | [], e :: t -> (
        match e + carry with
        | 0 -> 0 :: adder 0 (t, [])
        | 1 -> 1 :: adder 0 (t, [])
        | _ -> 0 :: adder 1 (t, []))
    | e1 :: t1, e2 :: t2 -> (
        match e1 + e2 + carry with
        | 0 -> 0 :: adder 0 (t1, t2)
        | 1 -> 1 :: adder 0 (t1, t2)
        | 2 -> 0 :: adder 1 (t1, t2)
        | _ -> 1 :: adder 1 (t1, t2))
  in
  let checked_zero l =
    let rec loop = function [] -> true | e :: t -> e = 0 && loop t in
    if loop l then [] else l
  in
  checked_zero (adder 0 (nA, nB))

(** Difference of two naturals.
    UNSAFE: First entry is assumed to be bigger than second.
    @param nA Natural.
    @param nB Natural.
*)
let diff_n nA nB =
  let checked_zero l =
    let rec loop = function [] -> true | e :: t -> e = 0 && loop t in
    if loop l then [] else l
  in
  let shift nat num =
    let rec build t = function 0 -> t | n -> 0 :: build t (n - 1) in
    build nat num
  in
  let ( ^ ) natA natB =
    let rec loop = function
      | [], [] -> []
      | e :: t, [] | [], e :: t -> (e lxor 0) :: loop (t, [])
      | e1 :: t1, e2 :: t2 -> (e1 lxor e2) :: loop (t1, t2)
    in
    checked_zero (loop (natA, natB))
  in

  let ( & ) natA natB =
    let rec loop = function
      | [], [] -> []
      | e :: t, [] | [], e :: t -> (e land 0) :: loop (t, [])
      | e1 :: t1, e2 :: t2 -> (e1 land e2) :: loop (t1, t2)
    in
    loop (natA, natB)
  in
  let inv nat =
    let rec loop = function [] -> [] | e :: t -> (e lxor 1) :: loop t in
    loop nat
  in
  let rec differ = function
    | x, [] -> x
    | x, y ->
        let borrow = inv x & y in
        differ (x ^ y, checked_zero (shift borrow 1))
  in
  differ (nA, nB)

(** Addition of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
 *)
let add_b bA bB =
  let checked_zero_b l =
    match l with
    | [] -> []
    | e :: l ->
        let rec loop = function [] -> true | e :: t -> e = 0 && loop t in
        if loop l then [] else l
  in
  checked_zero_b
    (match (bA, bB) with
    | [], [] -> []
    | a, [] | [], a -> a
    | s1 :: t1, s2 :: t2 -> (
        match (s1, s2) with
        | 0, 0 | 1, 1 -> s1 :: add_n t1 t2
        | _ ->
            if bA >>= abs_b bB then s1 :: diff_n t1 t2 else s2 :: diff_n t2 t1))

(** Difference of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let diff_b bA bB =
  match bB with [] -> bA | s :: t -> add_b bA ((s lxor 1) :: t)

(** Shifts bitarray to the left by a given natural number.
    @param bA Bitarray.
    @param d Non-negative integer.
*)
let rec shift bA d =
  let checked_zero_b l =
    match l with
    | [] -> []
    | e :: l ->
        let rec loop = function [] -> true | e :: t -> e = 0 && loop t in
        if loop l then [] else l
  in
  let shift nat num =
    let rec build t = function 0 -> t | n -> 0 :: build t (n - 1) in
    build nat num
  in
  checked_zero_b (match bA with s :: t -> s :: shift t d | _ -> [])

(** Multiplication of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let mult_b bA bB =
  match (bA, bB) with
  | [], [] | [], _ | _, [] -> []
  | s1 :: self_nat, s2 :: rhs ->
      let sign = s1 lxor s2 in
      let rec loop new_rhs =
        if new_rhs = [] then []
        else
          let rhs_sub1 = diff_n new_rhs [ 1 ] in
          add_n self_nat (loop rhs_sub1)
      in
      sign :: loop rhs

(** Quotient of two bitarrays.
    @param bA Bitarray you want to divide by second argument.
    @param bB Bitarray you divide by. Non-zero!
*)
let quot_b bA bB =
  let checked_zero l =
    let rec loop = function [] -> true | e :: t -> e = 0 && loop t in
    if loop l then [] else l
  in
  match (bA, bB) with
  | _, [] -> invalid_arg "Division by zero"
  | s :: t, [ 1; 1 ] -> (s lxor 1) :: t
  | l1, [ 0; 1 ] -> l1
  | s1 :: orig_rem, s2 :: rhs ->
      let sign = s1 lxor s2 in
      let rec loop rem =
        if rem >=! rhs then
          (* let _ = Printf.printf "%d\n" (List.length rem) in *)
          let new_rem = diff_n rem rhs in
          let q, _ = loop new_rem in
          (add_n [ 1 ] q, new_rem)
        else ([], rem)
      in
      let q, r = loop orig_rem in
      if sign = 1 && r <> [] then sign :: add_n q [ 1 ] else sign :: q
  | _, _ -> failwith "should not go here"

(** Modulo of a bitarray against a positive one.
    @param bA Bitarray the modulo of which you're computing.
    @param bB Bitarray which is modular base.
 *)
let mod_b bA bB = abs_b (diff_b ((mult_b bB  (quot_b bA bB)) ) bA)

(** Integer division of two bitarrays.
    @param bA Bitarray you want to divide.
    @param bB Bitarray you wnat to divide by.
*)
let div_b bA bB = (quot_b bA bB, mod_b bA bB)
