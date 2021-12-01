(** Ciphers
    Builtin integer based ciphers.
*)

open Builtin
open Basic_arithmetics
open Power

(********** Cesar Cipher **********)

(** Cesar's cipher encryption
    @param k is an integer corresponding to key
    @param m word to cipher.
    @param b base ; for ASCII codes should be set to 256.
 *)
let encrypt_cesar k m b =
  let rec loop = function [] -> [] | e :: t -> modulo (e + k) b :: loop t in
  loop m

(** Cesar's cipher decryption
    @param k is an integer corresponding to key
    @param m encrypted word.
    @param b base ; for ASCII code should be set to 256.
 *)
let decrypt_cesar k m b =
  let rec loop = function
    | [] -> []
    | e :: t ->
        (let v = e - k in
         if v < 0 then b + v else modulo v b)
        :: loop t
  in
  loop m

(********** RSA Cipher **********)

(** Generate an RSA ciphering keys.
    Involved prime numbers need to be distinct. Output is a couple
    of public, private keys.
    @param p prime number
    @param q prime number
*)
let generate_keys_rsa p q =
  let rec egcd a b =
    if a = 0 then (b, 0, 1)
    else
      let g, x1, y1 = egcd (modulo b a) a in
      (g, y1 - (quot b a * x1), x1)
  in
  let n = p * q and phi = (p - 1) * (q - 1) in
  let d =
    let g, x, y = egcd 65537 phi in
    modulo x phi
  in
  ((n, 65537), (n, d))

(** Encryption using RSA cryptosystem.
    @param m integer hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
 *)
let encrypt_rsa m (n, e) = mod_power m e n

(** Decryption using RSA cryptosystem.
    @param m integer hash of encrypter message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
 *)
let decrypt_rsa m (n, d) = mod_power m d n

(********** ElGamal Cipher **********)

(** Generate ElGamal public data. Generates a couple (g, p)
    where p is prime and g primitive root in F_p.
    @param p is prime having form 2*q + 1 for prime q.
 *)
let rec public_data_g p =
  let find_prim_root () =
    let factors n =
      let rec aux d n =
        if n = 1 then []
        else if n mod d = 0 then d :: aux d (n / d)
        else aux (d + 1) n
      in
      aux 2 n
    in
    let s = factors (p - 1) in
    let rec loop = function
      | n when n > p -> 0
      | n ->
          let rec loop_s = function
            | [] -> false
            | e :: t -> mod_power n (quot (p - 1) e) p = 1 || loop_s t
          in
          if loop_s s then loop (n + 1) else n
    in
    loop 2
  in
  (find_prim_root (), p)

(** Generate ElGamal public and private keys.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let generate_keys_g (g, p) =
  let rec find_key = function
    | k when k > p ->
        failwith "Something went wrong in find_key (generate_keys_g)"
    | k -> if gcd k p = 1 then k else find_key (k + 1)
  in
  let x = find_key 2 in
  (mod_power g x p, x)

(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)
let encrypt_g msg (g, p) kA =
  let rec find_key = function
    | k when k > p -> failwith "Something went wrong in find_key (encrypt_g)"
    | k -> if gcd k p = 1 then k else find_key (k + 1)
  in
  let k = find_key 3 in
  let p2 = mod_power g k p and s = mod_power kA k p in
  (p2, modulo (msg * s) p)

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let decrypt_g (msgA, msgB) a (g, p) =
  let sp = mod_power msgA a p in
  quot msgB sp
