From QuickChick Require Import QuickChick.
Require Import ZArith Strings.Ascii Strings.String.
From ExtLib.Structures Require Import Functor Applicative.
Require Import Bool BinPos BinNat PeanoNat Nnat.
Require Import Coq.Lists.List.
Import ListNotations.
Require Import Coq.Arith.PeanoNat.
Require Import Coq.Arith.Arith.

Check RandomSeed.

CoInductive RandomSeedTree :=
| RstNode : RandomSeed -> RandomSeedTree -> RandomSeedTree -> RandomSeedTree.

Definition root_rst (rst : RandomSeedTree) : RandomSeed :=
  match rst with
  | RstNode root _ _ => root
  end.

Definition left_rst (rst : RandomSeedTree) : RandomSeedTree :=
  match rst with
  | RstNode _ t1 _ => t1
  end.

Definition right_rst (rst : RandomSeedTree) : RandomSeedTree :=
  match rst with
  | RstNode _ _ t2 => t2
  end.

CoFixpoint mkSeedTree (s : RandomSeed) : RandomSeedTree :=
  let (s1, s2) := randomSplit s in
  RstNode s (mkSeedTree s1) (mkSeedTree s2).

Extract Constant RandomSeed   => "Random.State.t".
Extract Constant randomNext   => "(fun r -> Random.State.bits r, r)".
(* Extract Constant rndGenRange => "SR.genRange".*)
Extract Constant randomSplit  => "(fun x -> (x,x))".
Extract Constant mkRandomSeed => "(fun x -> Random.init x; Random.get_state())".
Extract Constant randomRNat  =>
  "(fun (x,y) r -> if y < x then failwith ""choose called with unordered arguments"" else  (x + (Random.State.int r (y - x + 1)), r))".
Extract Constant randomRBool => "(fun _ r -> Random.State.bool r, r)".
Extract Constant randomRInt  =>
  "(fun (x,y) r -> if y < x then failwith ""choose called with unordered arguments"" else  (x + (Random.State.int r (y - x + 1)), r))".
Extract Constant randomRN =>
  "(fun (x,y) r -> if y < x then failwith ""choose called with unordered arguments"" else  (x + (Random.State.int r (y - x + 1)), r))".
Extract Constant newRandomSeed => "(Random.State.make_self_init ())".

Extract Constant show_N =>
  "(fun i ->
  let s = string_of_int i in
  let rec copy acc i =
    if i < 0 then acc else copy (s.[i] :: acc) (i-1)
  in copy [] (String.length s - 1))".

(*Extract Constant init =>
  "let _ = Random.self_init()".

Extract Constant getNRandoms =>
  "let rec n_rnums n r lower upper =
    let p = randomRNat (lower, upper) (mkRandomSeed r)
    in if n==0 then [fst p] else (fst p)::n_rnums (n-1) (fst p) lower upper".
*)

(*Fixpoint n_rnums (n : nat) (r:Z) (lower : nat) (upper : nat) :=
  let p := randomRNat (lower, upper) (mkRandomSeed r) in
  if beq_nat n 0 then [fst p] else (fst p)::n_rnums (n-1) (fst p) lower upper.
*)

(**    Begin attempt   **)
(*CoInductive GeneratedNats :=
  | lNat : list nat -> GeneratedNats
  | singleNat : nat -> list nat -> GeneratedNats.

CoFixpoint n_rnums (n : nat) (r : nat) (lower : nat) (upper : nat) : GeneratedNats :=
  let p := randomRNat (lower, upper) (mkRandomSeed (Z.of_nat r)) in
  if beq_nat n 0 then lNat [fst p] else singleNat (fst p) (n_rnums (n-1) (fst p) lower upper).
*)
(**   End attempt     **)

(* Params represents the policy of the password generation
  (N, l, u, d, s) where:
    - N represents the length of the password
    - l represents the number of lowercase letters in the password
    - u represents the number of uppercase letters in the password
    - d represents the number of digits in the password
    - s represents the number of symbols in the password
 *)

Definition params : (nat * nat * nat* nat * nat) := (20, 5, 5, 5, 5).

(* Number to Ascii extracted from https://coq.inria.fr/stdlib/Coq.Strings.Ascii.html *)
Definition ascii_of_pos : positive -> ascii :=
 let loop := fix loop n p :=
   match n with
     | O => zero
     | S n' =>
       match p with
         | xH => one
         | xI p' => shift true (loop n' p')
         | xO p' => shift false (loop n' p')
       end
   end
  in loop 8.

Definition ascii_of_N (n : N) :=
  match n with
    | N0 => zero
    | Npos p => ascii_of_pos p
  end.

Definition ascii_of_nat (a : nat) := ascii_of_N (N.of_nat a).

Fixpoint convertToString (asciiCodes : list nat) : string :=
  match asciiCodes with
    | [] => ""
    | h :: t => (String (ascii_of_nat h) "") ++ convertToString t
  end.

(* Get elements of the tuple *)
Definition getLength (params : (nat * nat * nat* nat * nat)) :=
  fst (fst (fst (fst params))).

Definition getNumberOfLowercases (params : (nat * nat * nat* nat * nat)) :=
  snd (fst (fst (fst params))).

Definition getNumberOfUppercases (params : (nat * nat * nat* nat * nat)) :=
  snd (fst (fst params)).

Definition getNumberOfDigits (params : (nat * nat * nat* nat * nat)) :=
  snd (fst params).

Definition getNumberOfSymbols (params : (nat * nat * nat* nat * nat)) :=
  snd params.

(* Produce parts of the password *)
Fixpoint produceLowercase (size : nat) : string :=
  let lower := 97 in
  let upper := 122 in
  (* convertToString (n_rnums size (Random.int upper) lower upper) *)
  "lowercase".

Fixpoint produceUppercase (size : nat) : string :=
  let lower := 65 in
  let upper := 90 in
  (* convertToString (n_rnums size (Random.int upper) lower upper) *)
  "uppercase".

Fixpoint produceDigits (size : nat) : string :=
  let lower := 48 in
  let upper := 57 in
  (* convertToString (n_rnums size (Random.int upper) lower upper) *)
  "digits".

Fixpoint produceSymbols (size : nat) : string :=
  let lower := 33 in
  let upper := 47 in
  (* convertToString (n_rnums size (Random.int upper) lower upper) *)
  "symbols".

Definition producePassword (params : (nat * nat * nat* nat * nat)) : string :=
  let totalCharacters := getLength params in
  let numberOfLowercases := getNumberOfLowercases params in
  let numberOfUppercases := getNumberOfUppercases params in
  let numberOfDigits := getNumberOfDigits params in
  let numberOfSymbols := getNumberOfSymbols params in
  let totalElements := numberOfLowercases + numberOfUppercases + numberOfDigits + numberOfSymbols in
  if (negb(totalCharacters <=? totalElements)) || (beq_nat totalCharacters totalElements)
  then
    let remainingCharacters := totalCharacters - (numberOfLowercases + numberOfUppercases + numberOfDigits + numberOfSymbols) in
    let lowercase := produceLowercase (getNumberOfLowercases params) in
    let uppercase := produceUppercase (getNumberOfUppercases params) in
    let digits := produceDigits (getNumberOfDigits params) in
    let symbols := produceSymbols (remainingCharacters + (getNumberOfSymbols params)) in
    lowercase ++ uppercase ++ digits ++ symbols
  else "There is an error in the initial tuple."
.

Infix "^" := appn (at level 30, right associativity) : fun_scope.

Extraction "passwordGeneration1.ml" mkRandomSeed randomRNat convertToString producePassword.

Example list1 := [48; 86; 122; 35; 72; 123; 114; 87; 60; 65; 117].
Example list2 := [60; 65;117;79;100;89;47;121;77;67;49].
Example list3 := [59;79;100;89;47;121;77;67;49;58;90].

Compute generatePassword list1.
Compute generatePassword list2.
Compute generatePassword list3.