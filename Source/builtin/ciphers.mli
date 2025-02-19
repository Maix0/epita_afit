(** Ciphers
    Builtin integer based ciphers.
*)

(********** Cesar Cipher **********)

val encrypt_cesar : int -> int list -> int -> int list
(** Cesar's cipher encryption
    @param k is an integer corresponding to key
    @param m word to cipher.
    @param b base ; for ASCII codes should be set to 256.
 *)

val decrypt_cesar : int -> int list -> int -> int list
(** Cesar's cipher decryption
    @param k is an integer corresponding to key
    @param m encrypted word.
    @param b base ; for ASCII code should be set to 256.
 *)

(********** RSA Cipher **********)

val generate_keys_rsa : int -> int -> (int * int) * (int * int)
(** Generate an RSA ciphering keys.
    Involved prime numbers need to be distinct. Output is a couple
    of public, private keys.
    @param p prime number
    @param q prime number
*)

val encrypt_rsa : int -> int * int -> int
(** Encryption using RSA cryptosystem.
    @param m integer hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
 *)

val decrypt_rsa : int -> int * int -> int
(** Decryption using RSA cryptosystem.
    @param m integer hash of encrypter message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
 *)

(********** ElGamal Cipher **********)

val public_data_g : int -> int * int
(** Generate ElGamal public data. Generates a couple (g, p)
    where p is prime and g primitive root in F_p.
    @param p is prime having form 2*q + 1 for prime q.
 *)

val generate_keys_g : int * int -> int * int
(** Generate ElGamal public and private keys.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)

val encrypt_g : int -> int * int -> int -> int * int
(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)

val decrypt_g : int * int -> int -> int * int -> int
(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
