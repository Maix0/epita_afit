(** A naive implementation of big integers

This module aims at creating a set of big integers naively. Such data
types will be subsequently called bitarrays. A bitarray is a list of
zeros and ones ; first integer representing the sign bit. In this
context zero is reprensented by the empty list []. The list is to
be read from left to right ; this is the opposite convention to the
one you usually write binary decompositions with. After the sign bit
the first encountered bit is the coefficient in front of two to
the power zero. This convention has been chosen to ease writing
down code. A natural bitarray is understood as being a bitarray of
which you've taken out the sign bit, it is just the binary
decomposition of a non-negative integer.

 *)

val from_int : int -> int list
(** Creates a bitarray from a built-in integer.
    @param x built-in integer.
*)

val to_int : int list -> int
(** Transforms bitarray of built-in size to built-in integer.
    UNSAFE: possible integer overflow.
    @param bA bitarray object.
 *)

val print_b : int list -> unit
(** Prints bitarray as binary number on standard output.
    @param bA a bitarray.
 *)

(** Internal comparisons on bitarrays and naturals. Naturals in this
    context are understood as bitarrays missing a bit sign and thus
    assumed to be non-negative.
*)

val compare_n : int list -> int list -> int
(** Comparing naturals. Output is 1 if first argument is bigger than
    second -1 if it is smaller and 0 in case of equality.
    @param nA A natural, a bitarray having no sign bit.
           Assumed non-negative.
    @param nB A natural.
 *)

val ( >>! ) : int list -> int list -> bool
(** Bigger inorder comparison operator on naturals. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)

val ( <<! ) : int list -> int list -> bool
(** Smaller inorder comparison operator on naturals. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)

val ( >=! ) : int list -> int list -> bool
(** Bigger or equal inorder comparison operator on naturals. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)

val ( <=! ) : int list -> int list -> bool
(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)

val compare_b : int list -> int list -> int
(** Comparing two bitarrays. Output is 1 if first argument is bigger
    than second -1 if it smaller and 0 in case of equality.
    @param bA A bitarray.
    @param bB A bitarray.
*)

val ( << ) : int list -> int list -> bool
(** Bigger inorder comparison operator on bitarrays. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)

val ( >> ) : int list -> int list -> bool
(** Smaller inorder comparison operator on bitarrays. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)

val ( <<= ) : int list -> int list -> bool
(** Bigger or equal inorder comparison operator on bitarrays. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)

val ( >>= ) : int list -> int list -> bool
(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)

val sign_b : int list -> int
(** Sign of a bitarray.
    @param bA Bitarray.
*)

val abs_b : int list -> int list
(** Absolute value of bitarray.
    @param bA Bitarray.
*)

val add_n : int list -> int list -> int list
(** Addition of two naturals, output is a natural.
    @param nA Natural.
    @param nB Natural.
*)

val diff_n : int list -> int list -> int list
(** Difference of two naturals, output is a natural.
    UNSAFE: First entry is assumed to be bigger than second.
    @param nA Natural.
    @param nB Natural.
*)

val add_b : int list -> int list -> int list
(** Addition of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
 *)

val diff_b : int list -> int list -> int list
(** Difference of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)

val shift : int list -> int -> int list
(** Shifts bitarray to the left by a given natural number.
    @param bA Bitarray.
    @param d Non-negative integer.
*)

val mult_b : int list -> int list -> int list
(** Multiplication of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)

val quot_b : int list -> int list -> int list
(** Quotient of two bitarrays.
    @param bA Bitarray you want to divide by second argument.
    @param bB Bitarray you divide by. Non-zero!
*)

val mod_b : int list -> int list -> int list
(** Modulo of a bitarray against a positive one.
    @param bA Bitarray the modulo of which you're computing.
    @param bB Bitarray which is modular base.
 *)

val div_b : int list -> int list -> int list * int list
(** Integer division of two bitarrays.
    @param bA Bitarray you want to divide.
    @param bB Bitarray you wnat to divide by.
*)
