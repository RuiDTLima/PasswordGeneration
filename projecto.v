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
  "let rec n_rnums n r =
    let p = randomRNat (33,126) (mkRandomSeed r)
    in if n==0 then [fst p] else (fst p)::n_rnums (n-1) (fst p)".
*)
(* Extraction "test.ml" randomRNat mkRandomSeed show_N. *)

(* Params represents the policy of the password generation
  (N, l, u, d, s) where:
    - N represents the length of the password
    - l represents the number of lowercase letters in the password
    - u represents the number of uppercase letters in the password
    - d represents the number of digits in the password
    - s represents the number of symbols in the password
 *)

Definition params : (nat * nat * nat* nat * nat) := (20, 5, 5, 5, 5).

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
  "lowercase".

Fixpoint produceUppercase (size : nat) : string :=
  "uppercase".

Fixpoint produceDigits (size : nat) : string :=
  "digits".

Fixpoint produceSymbols (size : nat) : string :=
  "symbols".

Definition producePassword (params : (nat * nat * nat* nat * nat)) : string :=
  let totalCharacters := getLength params in
  let numberOfLowercases := getNumberOfLowercases params in
  let numberOfUppercases := getNumberOfUppercases params in
  let numberOfDigits := getNumberOfDigits params in
  let numberOfSymbols := getNumberOfSymbols params in
  if beq_nat totalCharacters (numberOfLowercases + numberOfUppercases + numberOfDigits + numberOfSymbols) then
    let lowercase := produceLowercase (getNumberOfLowercases params) in
    let uppercase := produceUppercase (getNumberOfUppercases params) in
    let digits := produceDigits (getNumberOfDigits params) in
    let symbols := produceSymbols (getNumberOfSymbols params) in
    lowercase ++ uppercase ++ digits ++ symbols
  else "Change to optional return"
.

Compute producePassword params.

Fixpoint getNRandoms (n:nat) (r:Z) :=
  let p := randomRNat (33, 126) (mkRandomSeed r) in
    match n with
      | 0 => [fst p]
      | S n' => (fst p) :: getNRandoms n' (fst p)
    end.

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

Compute 5 + 3.
Definition ascii_of_nat (a : nat) := ascii_of_N (N.of_nat a).

Compute ascii_of_N 65.

Fixpoint generatePassword (asciiCodes : list nat) : string :=
  match asciiCodes with
    | [] => ""
    | h :: t => (String (ascii_of_nat h) "") ++ generatePassword t
  end.


Extraction "f.ml" generatePassword.

Example list1 := [48; 86; 122; 35; 72; 123; 114; 87; 60; 65; 117].
Example list2 := [60; 65;117;79;100;89;47;121;77;67;49].
Example list3 := [59;79;100;89;47;121;77;67;49;58;90].

Compute generatePassword list1.
Compute generatePassword list2.
Compute generatePassword list3.