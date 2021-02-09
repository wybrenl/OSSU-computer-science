(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(*Q1a*)
(* string * string list -> NONE or SOME lst*)
(* produces NONE if the string is not in the list, else returns SOME lst where lst is identical to the argument list except the string is not in it*)
fun all_except_option (string, stringlist) =
              case stringlist of
                [] => NONE
               | x::xs' =>
                 if same_string(x, string)
                 then SOME xs'
                 else
                     case all_except_option(string, xs') of
                         NONE => NONE
                     |  SOME xs' => SOME(x::xs');

(*Q1b *)
(*string list list * string -> string list *)
(*produces a list of string of every word but the string word in the string list list*)

fun get_substitutions1 (stringlists, string) =
    case stringlists of
        [] => []
      | x::xs' => case all_except_option (string, x) of
                      NONE => get_substitutions1 (xs',string)
                    | SOME xs => xs @ get_substitutions1 (xs', string);

(*Q1c *)
(*string list list * string -> string list *)
(*Q1b made tail recursive*)

fun get_substitutions2 (stringlists, string) =
    let fun aux(stringlists, string, acc) =
            case stringlists of
                [] => acc
              | x::xs' => case all_except_option(string, x) of
                              NONE => aux(xs', string, acc)
                            | SOME xs => aux (xs', string, acc @ xs)
    in
        aux (stringlists, string, [])
    end

(*Q1d) *)
(* string list list * string*string*string -> string*string*string list) *)
(* produces a list of similar full names by substituting based on the first name*)
(*
fun similar_names (stringlists, fullname) =
    let
        string = first (fullname)
    case stringlists of
        [] => [fullname]
     |  x::xs' => case get_substitutions1 ()*)

fun similar_names (stringlists, fullname) =
    let
        val {first=f, middle=m, last=l} = fullname
        fun make_full_name namelist =
            case namelist of
                [] => []
              | x::xs' => {first=x, middle=m, last=l}::(make_full_name(xs'))
    in
        fullname::make_full_name(get_substitutions1(stringlists, f))
    end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)
(*Q2a *)
(* card -> color) *)
(* produces the color of a given card*)
fun card_color (suit, rank) =
    case suit of
        Clubs => Black
      | Spades => Black
      | Diamonds => Red
      | Hearts => Red;

(*Q2b *)
(*card -> int *)
(*produces the value of a given card*)
fun card_value (suit, rank) =
    case rank of
        Ace => 11
      | Num i => i
      | _ => 10;

(*Q2c *)
(* card list * card * exception -> card list) *)
(* remove card from card list, in case of duplicates remove only the first card,
in case c does not exist raise exception*)

fun remove_card (cardlist, card, e) =
    case cardlist of
        [] => raise e
      | x::xs' => if x = card
                  then xs'
                  else x::remove_card (xs', card, e);
(*Q2d *)
(*card list -> bool *)
(*produces true if all cards are of the same color, else false*)
fun all_same_color cardlist =
    case cardlist of
        [] => true
      | _::[] => true
      | x::xs' => case xs' of
                      [] => true
                    | y::ys' => card_color x = card_color y andalso all_same_color xs';

(*Q2e *)
(*card list -> int *)
(*produces the sum of the cards held*)
fun sum_cards cardlist =
    case cardlist of
        [] => 0
      | x::xs' => card_value x + sum_cards xs';

(*Q2f *)
(*card list * int -> int *)
(*produces the score of the solitaire game*)
fun score (cardlist, goal) =
    let
        val sum = sum_cards cardlist
        val same = all_same_color cardlist
    in
        if sum = goal then 0
        else if sum < goal andalso same then (goal - sum) div 2
        else if sum > goal andalso same then 3*(sum-goal) div 2
        else if sum < goal then goal - sum
        else 3*(sum-goal)
    end;

(*Q2g *)
(* card list * move list * int -> int *)
(* produces the score at a given part of the game*)
fun officiate (cardlist, movelist, goal) =
    let
        fun next_hand (cardlist, heldlist, movelist) =
            case movelist of
                [] => score (heldlist, goal)
              | x::xs' => if sum_cards heldlist > goal then score (heldlist, goal)
                          else case x of
                                 Discard i => next_hand (cardlist, remove_card (heldlist, i, IllegalMove), movelist)
                               | Draw => case cardlist of
                                             [] => score (heldlist, goal)
                                           | y::ys' => next_hand (remove_card (cardlist, y, IllegalMove), y::heldlist, xs')
    in
        next_hand (cardlist, [], movelist)
    end
