(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let
	val r = g f1 f2
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(*Q1*)
(* string list -> string list*)
(* returns a string list that has only the strings in the argument that start with an uppercase letter *)

val only_capitals = List.filter(fn x => Char.isUpper(String.sub(x,0)))

(*Q2*)
(* string list -> string*)
(* returns the longest string in a string list*)

val longest_string1 = foldl(fn (s, acc) => if String.size s > String.size acc then s else acc) ""

(*Q3*)
(* string list -> string*)
(* returns the longest string in a string list*)

val longest_string2 = foldl(fn (s, acc) => if String.size s >= String.size acc then s else acc) ""

(*Q4*)
(*string list -> string*)
(**)

fun longest_string_helper f = foldl(fn (s, acc) => if f(String.size s, String.size acc) then s else acc) "";

val longest_string3 = longest_string_helper (fn (s, acc) => s > acc);
val longest_string4 = longest_string_helper (fn (s, acc) => s>= acc);

(*Q5*)
(* string list -> string *)
(* returns the longest string that starts with a capitalized letter, "" otherwise*)

val longest_capitalized = longest_string1 o only_capitals

(*Q6*)
(* string -> string*)
(* returns a string that is the same characters in reverse order*)

val rev_string = implode o rev o explode

(*Q7*)
(*(’a -> ’b option) -> ’a list -> ’b *)
(*returns SOME v for first occurance of v, NONE + eception No answer otherse*)

fun first_answer f list = 
	case list of
		[] => raise NoAnswer
		|x::xs' => if f x = SOME x then x else first_answer f xs'

(*Q8*)
(*’a -> ’b list option) -> ’a list -> ’b list option *)
(**)

fun all_answers f list = 
	let 
		fun aux list acc =
			case list of
				[] => SOME (acc)
				| x::xs' => case f x of
								NONE => NONE
								| SOME x => aux xs' (x@acc)
	in 
		aux list []
	end

(*Q9a*)
(**)
(**)

val count_wildcards = g (fn () => 1) (fn _ => 0)
val count_wild_and_variable_lengths = g (fn () => 1) String.size
fun count_some_var (string, pattern) = g (fn () => 0) (fn x => if x = string then 1 else 0) pattern  
val list_strings = g (fn () => []) (fn x => x)

(*Q10*)
(*pattern -> bool*)
(*returns true if all variables in the pattern are distinct from each other*)
(*
fun check_pat pattern = 
	let 
		fun list_strings pattern = 
			g (fn () => []) (fn x => x) pattern
*)

(**)
(**)
(**)

(**)
(**)
(**)