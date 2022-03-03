(* An implementation of the RSA encryption scheme *)

module Math = struct
  let rec gcd : int -> int -> int
    = fun a b ->
    if b = 0 then max a (-a)
    else gcd b (a mod b)

  (* extended_gcd a b = (x, y) such that a*x + b*y = gcd a b *)
  let rec extended_gcd : int -> int -> (int * int)
    = fun a b ->
    if (a mod b) = 0
    then (0, 1)
    else let (x,y) = extended_gcd b (a mod b) in
         (y, (x - (y * (a / b))))

  (* modulo_inverse a n = b such that a*b = 1 (mod n) *)
  let modulo_inverse : int -> int -> int
    = fun a n ->
    let (x,_) = extended_gcd a n in
    x mod n

  (* totient n = (p - 1) * (q - 1) where n = p * q, such that both p
     and q are primes *)
  let totient : int -> int -> int
    = fun p q -> (p - 1) * (q - 1)

  let square : int -> int
    = fun x -> x * x

  let odd : int -> bool
    = fun x -> x mod 2 = 1

  (* modulo_power base exp n = base^exp (mod n) *)
  let rec modulo_power : int -> int -> int -> int
    = fun base exp n ->
    if exp = 0 then 1
    else if odd exp
         then (base * modulo_power base (exp - 1) n) mod n
         else (square (modulo_power base (exp / 2) n)) mod n
end

module RSA = struct
  (* Tests whether a given exponent is a valid public exponent *)
  let is_public_exponent : int -> int -> int -> bool
    = fun exp p q ->
    let open Math in
    exp > 1 && exp < (totient p q) && (gcd exp (totient p q)) = 1

  (* The private exponent is the inverse of the public exponent *)
  let private_exponent : int -> int -> int -> int
    = fun exp p q ->
    if is_public_exponent exp p q
    then Math.modulo_inverse exp (Math.totient p q)
    else raise (Invalid_argument "The public exponent is not compatible with the given modulus.")

  (* An encrypted message is c = m^e (mod n) *)
  let encrypt : int -> int -> int -> int
    = fun m exp n ->
    if m > n then raise (Invalid_argument "The message is larger than the modulus.")
    else Math.modulo_power m exp n

  (* A decrypted message is m = c^d (mod n) *)
  let decrypt : int -> int -> int -> int
    = fun c d n -> Math.modulo_power c d n
end

let example () =
  let p = 41 (* A large prime *) in
  let q = 47 (* A large prime different from p *) in
  let n = p * q (* The public modulus *) in
  let exp = 7 (* The public exponent *) in
  let d = RSA.private_exponent exp p q in
  let message = 42 in
  let encrypted_message = RSA.encrypt message exp n in
  let decrypted_message = RSA.decrypt encrypted_message d n in
  Printf.printf "Message   = %d\n" message;
  Printf.printf "Encrypted = %d\n" encrypted_message;
  Printf.printf "Decrypted = %d\n%!" decrypted_message
