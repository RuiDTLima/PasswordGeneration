(** val fst : ('a1 * 'a2) -> 'a1 **)

let fst = function
| (x, _) -> x

(** val snd : ('a1 * 'a2) -> 'a2 **)

let snd = function
| (_, y) -> y

(** val add : int -> int -> int **)

let rec add = (+)

module Nat =
 struct
 end

module Pos =
 struct
  (** val succ : int -> int **)

  let rec succ = Pervasives.succ

  (** val of_succ_nat : int -> int **)

  let rec of_succ_nat n =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> 1)
      (fun x -> succ (of_succ_nat x))
      n
 end

module N =
 struct
  (** val of_nat : int -> int **)

  let of_nat n =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> 0)
      (fun n' -> (Pos.of_succ_nat n'))
      n
 end

(** val zero : char **)

let zero = '\000'

(** val one : char **)

let one = '\001'

(** val shift : bool -> char -> char **)

let shift = fun b c -> Char.chr (((Char.code c) lsl 1) land 255 + if b then 1 else 0)

(** val append : char list -> char list -> char list **)

let rec append s1 s2 =
  match s1 with
  | [] -> s2
  | c::s1' -> c::(append s1' s2)

type randomSeed = Random.State.t

(** val mkRandomSeed : int -> randomSeed **)

let mkRandomSeed = (fun x -> Random.init x; Random.get_state())

(** val randomRNat : (int * int) -> randomSeed -> int * randomSeed **)

let randomRNat = (fun (x,y) r -> if y < x then failwith "choose called with unordered arguments" else  (x + (Random.State.int r (y - x + 1)), r))

(* Initialize Random so it won't use the default seed *)
let _ = Random.self_init()

(* This function receives a number n and an initial seed (int). It yelds a list of (n+1) random ints between 1 and 50 *)
let rec n_rnums n r lower upper =
        let p = randomRNat (lower, upper) (mkRandomSeed r) in
        if n==0 then [fst p] else (fst p)::n_rnums (n-1) (fst p) lower upper

(** val ascii_of_pos : int -> char **)

let ascii_of_pos =
  let rec loop n p =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> zero)
      (fun n' ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun p' -> shift true (loop n' p'))
        (fun p' -> shift false (loop n' p'))
        (fun _ -> one)
        p)
      n
  in loop (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
       (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
       0))))))))

(** val ascii_of_N : int -> char **)

let ascii_of_N n =
  (fun f0 fp n -> if n=0 then f0 () else fp n)
    (fun _ -> zero)
    (fun p -> ascii_of_pos p)
    n

(** val ascii_of_nat : int -> char **)

let ascii_of_nat a =
  ascii_of_N (N.of_nat a)

(** val convertToString : int list -> char list **)

let rec convertToString = function
| [] -> ""
| h :: t -> ((Char.escaped (ascii_of_nat h)) ^ convertToString t)

(** val getLength : ((((int * int) * int) * int) * int) -> int **)

let getLength params =
  fst (fst (fst (fst params)))

(** val getNumberOfLowercases : ((((int * int) * int) * int) * int) -> int **)

let getNumberOfLowercases params =
  snd (fst (fst (fst params)))

(** val getNumberOfUppercases : ((((int * int) * int) * int) * int) -> int **)

let getNumberOfUppercases params =
  snd (fst (fst params))

(** val getNumberOfDigits : ((((int * int) * int) * int) * int) -> int **)

let getNumberOfDigits params =
  snd (fst params)

(** val getNumberOfSymbols : ((((int * int) * int) * int) * int) -> int **)

let getNumberOfSymbols =
  snd

(** val produceLowercase : int -> char list **)

let rec produceLowercase size =
        let lower = 97 in
        let upper = 122 in
        convertToString (n_rnums size (Random.int upper) lower upper)

(** val produceUppercase : int -> char list **)

let rec produceUppercase size =
        let lower = 65 in
        let upper = 90 in
        convertToString (n_rnums size (Random.int upper) lower upper)

(** val produceDigits : int -> char list **)

let rec produceDigits size =
        let lower = 48 in
        let upper = 57 in
        convertToString (n_rnums size (Random.int lower) lower upper)

(** val produceSymbols : int -> char list **)

let rec produceSymbols size =
        let lower = 33 in
        let upper = 47 in
        convertToString (n_rnums size (Random.int upper) lower upper)

(** val producePassword : ((((int * int) * int) * int) * int) -> char list **)

let producePassword params =
  let totalCharacters = getLength params in
  let numberOfLowercases = getNumberOfLowercases params in
  let numberOfUppercases = getNumberOfUppercases params in
  let numberOfDigits = getNumberOfDigits params in
  let numberOfSymbols = getNumberOfSymbols params in
  if (=) totalCharacters
       (add (add (add numberOfLowercases numberOfUppercases) numberOfDigits)
         numberOfSymbols)
  then let lowercase = produceLowercase (getNumberOfLowercases params) in
       let uppercase = produceUppercase (getNumberOfUppercases params) in
       let digits = produceDigits (getNumberOfDigits params) in
       let symbols = produceSymbols (getNumberOfSymbols params) in
       lowercase ^ (uppercase ^ (digits ^ symbols))
  else "Error"


let params = ((((20, 5), 5), 5), 5)

let () = print_string "Input the length of the password: ";; 
let length = read_int() in
print_string "Input the number of lowercases: ";;
let lowercases = read_int() in
print_string "Input the number of uppercases: ";;
let uppercases = read_int() in
print_string "Input the number of digits: ";;
let digits = read_int() in
print_string "Input the number of symbols: ";;
let symbols = read_int() in
let () = print_endline (producePassword ((((length, lowercases), uppercases), digits), symbols))
print_string "Finish"
(*
print_string "Input: ";;
read_line();;
  print_string "Hello"*)
