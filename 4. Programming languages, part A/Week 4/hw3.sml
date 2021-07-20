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

(**** you can put all your code here ****);
(*Q1 *)
(*string list -> string list *)
(*filters a list of string to only include string that start with a capital letter*)

val only_capitals = List.filter (fn x => Char.isUpper(String.sub(x,0)));

(*Q2 *)
(*string list -> string *)
(*produces the longest string from a stringlist, the first in case of a tie*)

val longest_string1 = List.foldl (fn (x,acc) => if String.size x > String.size acc then x else acc) "";

(*Q3 *)
(*string list -> string *)
(*produces the longest string from a stringlist, the last in case of ties *)

val longest_string2 = List.foldl (fn (x,acc) => if String.size x >= String.size acc then x else acc) "";

(*Q4 *)
(*(int * int -> bool) -> string list -> string* *)

fun longest_string_helper f = List.foldl (fn (x,acc) => if f(String.size x, String.size acc) then x else acc) "";
val longest_string3 = longest_string_helper (fn (x,acc) => x > acc)
val longest_string4 = longest_string_helper (fn (x,acc) => x >= acc);

(*Q5 *)
(*string list -> string *)
(*produces the longest string that starts with a capital from a list of string*)
val longest_capitalized = longest_string1 o only_capitals

(*Q6 *)
(*string -> string *)
(*produces the reverse of given string*);
val rev_string =  implode o rev o explode

(*Q7 *)
(* ('a -> 'b option) -> 'a list -> 'b *);
fun first_answer f xs =
    case xs of
        [] => raise NoAnswer
      | x::xs' => case f x of
                      NONE => first_answer f xs'
                   |  SOME x => x;

(*Q8 *)
(*('a -> 'b list option) -> 'a list -> 'b list option*)
fun all_answers f xs =
    let fun helper xs acc =
        case xs of
        [] => SOME acc
      | x::xs' => case f x of
                      NONE => NONE
                   |  SOME x => helper xs' (acc@x)
    in
        helper xs []
    end;

(*Q9a *)
(*pattern -> int *)
(*produces how many Wildcards a pattern contains*)
val count_wildcards = g (fn () => 1) (fn x => 0)


(*Q9b *)
(*pattern -> int *)
(*produces the bumber of Wildcard patterns plus the sum of the string lengths of all the virables in the variable pattern*)
val count_wild_and_variable_lengths = g (fn () => 1) (fn x => String.size x);

(*Q9c *)
(*string * pattern -> int *)
(*produces the number of times the string appears as a variable in the pattern*)
val count_some_var = fn (string,p) => g (fn x => 0) (fn x => if string = x then 1 else 0) p;

(*Q10 *)
(*pattern -> bool *)
(*returns true if and only if all the variables
(appearing in the pattern are distinct from each other*)
fun check_pat p =
    let
    fun return_strings p =
        case p of
             Variable x        => [x]
	       | TupleP ps         => List.foldl (fn (p,i) => return_strings p @ i) [] ps
	       | ConstructorP(_,p) => return_strings p
	       | _                 => [];
    fun check_repeats stringlist =
        case stringlist of
            [] => true
          | x::xs' => not (List.exists (fn y => y = x)  xs') andalso check_repeats xs'
    in
    check_repeats (return_strings p)
    end

(*Q11 *)
(*valu * pattern -> (string * valu) list option *)
fun match (v, p) =
	case (v, p) of
		(_, Wildcard)	 		 					 => SOME []
	  | (_, Variable s)   		 					 => SOME [(s, v)]
	  | (Unit, UnitP)        		 				 => SOME []
	  | (Const c1, ConstP c)  	 		 			 => if c = c1 then SOME [] else NONE
	  | (Tuple vs, TupleP ps)	         			 => if length vs = length ps
							  			       			then all_answers match (ListPair.zip (vs, ps))
											  			else NONE
	  | (Constructor (s2, v), ConstructorP (s1, p1)) => if s1 = s2 then match (v, p1) else NONE
	  | _ 					 						 => NONE

(*Q12 *)
(*value * pattern list -> (string * valu) list option *)
fun first_match v ps =
	SOME (first_answer (fn p => match (v, p)) ps)
	handle NoAnswer => NONE
