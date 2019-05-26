
type uint =
| Nil
| D0 of uint
| D1 of uint
| D2 of uint
| D3 of uint
| D4 of uint
| D5 of uint
| D6 of uint
| D7 of uint
| D8 of uint
| D9 of uint

(** val revapp : uint -> uint -> uint **)

let rec revapp d d' =
  match d with
  | Nil -> d'
  | D0 d0 -> revapp d0 (D0 d')
  | D1 d0 -> revapp d0 (D1 d')
  | D2 d0 -> revapp d0 (D2 d')
  | D3 d0 -> revapp d0 (D3 d')
  | D4 d0 -> revapp d0 (D4 d')
  | D5 d0 -> revapp d0 (D5 d')
  | D6 d0 -> revapp d0 (D6 d')
  | D7 d0 -> revapp d0 (D7 d')
  | D8 d0 -> revapp d0 (D8 d')
  | D9 d0 -> revapp d0 (D9 d')

(** val rev : uint -> uint **)

let rev d =
  revapp d Nil

module Little =
 struct
  (** val double : uint -> uint **)

  let rec double = function
  | Nil -> Nil
  | D0 d0 -> D0 (double d0)
  | D1 d0 -> D2 (double d0)
  | D2 d0 -> D4 (double d0)
  | D3 d0 -> D6 (double d0)
  | D4 d0 -> D8 (double d0)
  | D5 d0 -> D0 (succ_double d0)
  | D6 d0 -> D2 (succ_double d0)
  | D7 d0 -> D4 (succ_double d0)
  | D8 d0 -> D6 (succ_double d0)
  | D9 d0 -> D8 (succ_double d0)

  (** val succ_double : uint -> uint **)

  and succ_double = function
  | Nil -> D1 Nil
  | D0 d0 -> D1 (double d0)
  | D1 d0 -> D3 (double d0)
  | D2 d0 -> D5 (double d0)
  | D3 d0 -> D7 (double d0)
  | D4 d0 -> D9 (double d0)
  | D5 d0 -> D1 (succ_double d0)
  | D6 d0 -> D3 (succ_double d0)
  | D7 d0 -> D5 (succ_double d0)
  | D8 d0 -> D7 (succ_double d0)
  | D9 d0 -> D9 (succ_double d0)
 end

module Pos =
 struct
  (** val to_little_uint : int -> uint **)

  let rec to_little_uint p =
    (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
      (fun p0 -> Little.succ_double (to_little_uint p0))
      (fun p0 -> Little.double (to_little_uint p0))
      (fun _ -> D1 Nil)
      p

  (** val to_uint : int -> uint **)

  let to_uint p =
    rev (to_little_uint p)
 end

module Z =
 struct
  (** val of_N : int -> int **)

  let of_N = fun p -> p

  (** val to_int : int -> unit **)

  let to_int n =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> (fun _ -> ()) (D0 Nil))
      (fun p -> (fun _ -> ()) (Pos.to_uint p))
      (fun p -> (fun _ -> ()) (Pos.to_uint p))
      n
 end

(** val show_N : int -> char list **)

let show_N = (fun i ->
  let s = string_of_int i in
  let rec copy acc i =
    if i < 0 then acc else copy (s.[i] :: acc) (i-1)
  in copy [] (String.length s - 1))

type randomSeed = Random.State.t

(** val mkRandomSeed : int -> randomSeed **)

let mkRandomSeed = (fun x -> Random.init x; Random.get_state())

(** val randomRNat : (int * int) -> randomSeed -> int * randomSeed **)

let randomRNat = (fun (x,y) r -> if y < x then failwith "choose called with unordered arguments" else  (x + (Random.State.int r (y - x + 1)), r))




(* Initialize Random so it won't use the default seed *)
let _ = Random.self_init()

(* This function receives a number n and an initial seed (int). It yields a list of (n+1) random ints between 1 and 50 *)
let rec n_rnums n r lower upper =
  let p = randomRNat (lower, upper) (mkRandomSeed r)
  in if n==0 then [fst p] else (fst p)::n_rnums (n-1) (fst p) lower upper

(* We define a function to print a list of ints *)
let rec print_list = function 
  [] -> ()
  | e::l -> print_int e ; print_string " " ; print_list l

let () = print_list (n_rnums 10 (Random.int 100) 97 122)
