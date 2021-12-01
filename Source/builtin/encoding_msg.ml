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

let decode msg bits =
  let fullbits =
    let rec f = function 0 -> 1 | n -> f (n - 1) lor (1 lsl (n-1)) in
    f bits
  in
  let rec build_msg = function 
    | 0 -> ""
    | msg -> build_msg (msg lsr bits) ^ String.make 1 (Char.chr (msg land fullbits))
in build_msg msg
