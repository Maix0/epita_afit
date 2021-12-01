(** Encoding Strings *)

open Builtin
open Basic_arithmetics
open Power

(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7. *)

(** Decode a string containing ASCII characters.
    @param msg is an integer representing an encoded message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7. *)
let encode str bits =
  let rec loop_chr = function
    | -1 -> 0
    | n ->
        loop_chr (n - 1)
        lor (Char.code str.[String.length str - n - 1] lsl (n * bits))
  in
  loop_chr (String.length str - 1)

let decode msg bits = ""
