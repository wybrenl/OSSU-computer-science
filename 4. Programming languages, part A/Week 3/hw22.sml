(*Homework week 3*)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(*Q1a*)
(* string string list -> string list option*)
(*returns NONE if string is not in the list, returns SOME lst identical to original list but without the string*)

fun all_except_option(string, stringlist) = 
    case stringlist of 
        [] => NONE
        | x::xs' =>  if same_string(string, x) 
                     then SOME (xs') 
                     else 
                     case all_except_option(string, xs') of
                        NONE => NONE
                        | SOME xs' => SOME(x::xs')
                
(*Q1b*)
(* (string list) list  string -> string list*)
(* returns all strings that are in some string list*)

fun get_substitutions1 (stringlist, string) =
    case stringlist of
    [] => []
    | x::xs' => case all_except_option(string, x) of
                     NONE =>  get_substitutions1(xs', string)
                    |SOME x => x @ get_substitutions1(xs', string);

(*Q1c*)
(* (string list) list  string -> string list*)
(* returns all strings that are in some string list*)

fun get_substitutions2 (stringlist, string) =
    let fun aux (stringlist2, string2, acc) =
        case stringlist2 of
        [] => acc
        | x::xs' => case all_except_option(string2, x) of
                     NONE =>  aux(xs', string2, acc)
                    |SOME x => aux(xs', string2, acc@x);
    in
        aux(stringlist,string,[])
    end 

(*Q1d*)
(* (string list) (string*string*string) -> (string*string*string) list *)
(* returns a list of full names based on substitutions*)

fun similar_names (stringlist, fullname) =
    let 
        val {first=f, middle=m, last=l} = fullname
        fun make_names xs =
            case xs of 
            [] => []
            |x::xs' => {first=x, middle=m, last=l}::make_names(xs')
    in
        fullname::make_names(get_substitutions1(stringlist, f))
    end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(*Q2a*)
(* card -> color *)
(* returns the color of a card*)

fun card_color (card) = 
    case card of 
        (Spades,_) => Black
        | (Clubs,_) => Black
        | (_,_) => Red

(*Q2b*)
(* card -> value*)
(* returns the value of a card*)

fun card_value (card) =
    case card of
      (_,Ace) => 11
    | (_,Num i) => i
    | _ => 10

(*Q2c*)
(* card list card exception -> card list or exception*)
(* returns the card list without the card or exception*)

fun remove_card (cardlist, card, e) = 
    case cardlist of 
     [] => raise e
     | x::xs' => if x = card 
                then xs'
                else x::remove_card(xs', card, e)

(*Q2d*)
(* card list -> boolean*)
(* returns true if all cards are same color, false otherwise*)

fun all_same_color (cardlist) = 
    case cardlist of 
    [] => true
    | x::[] => true
    | x::y::ys' => card_color(x) = card_color(y) andalso all_same_color(y::ys')

(*Q2e*)
(* card list -> int *)
(* returns the sum of the card's values*)

fun sum_cards (cardlist) =
    let fun aux (list, acc) =
        case list of
            [] => acc
            | x::xs' => aux (xs', acc+card_value(x))
    in 
        aux (cardlist, 0)
    end 

(*Q2f*)
(* card list int -> int*)
(* returns the score of the held cards*)

fun score (cardlist, goal) =
    let 
        val prelim_score = if sum_cards(cardlist) > goal then 3*(sum_cards(cardlist)-goal) else goal - sum_cards(cardlist)
        val busted = if sum_cards(cardlist) > goal then "A" else "B"
        val same_color = all_same_color(cardlist)
        val score = (busted, same_color)
    in  
        case score of
        ("B", false) => prelim_score
        |("B", true) => prelim_score div 2
        |("A", false) => prelim_score
        |("A", true) => prelim_score div 2
    end  

(*Q2g*)
(* card list move list int -> int*)
(* returns the score based on a run of a game*)

fun officiate (cardlist, movelist, goal) = 
     let fun next_hand (cardlist, heldlist, movelist) =
            case movelist of
                [] => score (heldlist, goal)
                | x::xs' => if sum_cards (heldlist) > goal then score (heldlist, goal) 
                            else case x of
                                Discard c => next_hand (cardlist, remove_card(heldlist, c, IllegalMove), xs')
                                | Draw => case cardlist of 
                                                [] => score (heldlist, goal)
                                                | y::ys' => next_hand (ys', y::heldlist, xs')
    in 
        next_hand(cardlist, [], movelist)
    end

(**)
(**)
(**)

(**)
(**)
(**)