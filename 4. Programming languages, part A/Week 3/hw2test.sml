(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw22.sml";

val test1_1 = all_except_option ("string", ["string"]) = SOME []
val test1_2 = all_except_option ("string", ["string","thing"]) = SOME ["thing"]
val test1_3 = all_except_option ("string", ["thing"]) = NONE
val test1_4 = all_except_option ("string", ["thing","ding","string","ling"]) = SOME ["thing","ding","ling"];
val test1_5 = all_except_option ("string", ["thing","ding","ling"]) = NONE

val test2_1 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test2_2 = get_substitutions1 ([["foo","poo"],["there"]], "foo") = ["poo"]
val test2_3 = get_substitutions1 ([["foo","poo"],["there", "foo"]], "foo") = ["poo","there"]
val test2_4 = get_substitutions1 ([["foo","poo"],["there", "foo"],["bear"]], "foo") = ["poo","there"]
val test2_5 = get_substitutions1 ([[],[]], "foo") = []

val test3_1 = get_substitutions2 ([["foo"],["there"]], "foo") = []
val test3_2 = get_substitutions2 ([["foo","poo"],["there"]], "foo") = ["poo"]
val test3_3 = get_substitutions2 ([["foo","poo"],["there", "foo"]], "foo") = ["poo","there"]
val test3_4 = get_substitutions2 ([["foo","poo"],["there", "foo"],["bear"]], "foo") = ["poo","there"]
val test3_5 = get_substitutions2 ([[],[]], "foo") = []

val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

val test5_1 = card_color (Clubs, Num 2) = Black
val test5_2 = card_color (Spades, Num 2) = Black
val test5_3 = card_color (Diamonds, Num 2) = Red
val test5_4 = card_color (Hearts, Num 2) = Red


val test6_1 = card_value (Clubs, Ace) = 11
val test6_2 = card_value (Clubs, Jack) = 10
val test6_3 = card_value (Clubs, Num 2) = 2
val test6_4 = card_value (Clubs, Num 9) = 9

val test7_1 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val test7_2 = remove_card ([(Hearts, Ace),(Hearts,Ace)], (Hearts, Ace), IllegalMove) = [(Hearts, Ace)]
val test7_3 = remove_card ([(Hearts, Ace),(Spades,Num 2)], (Hearts, Ace), IllegalMove) = [(Spades, Num 2)]

val test8_1 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val test8_2 = all_same_color [(Hearts, Ace), (Spades, Ace)] = false
val test8_3 = all_same_color [(Hearts, Ace), (Hearts, Ace), (Spades, Ace)] = false

val test9_1 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4
val test9_2 = sum_cards [(Clubs, Num 2),(Clubs, Jack)] = 12

val test10_1 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
val test10_2 = score ([(Hearts, Num 2),(Hearts, Num 4)],10) = 2
val test10_3 = score ([(Hearts, Num 2),(Clubs, Jack)],10) = 6
val test10_4 = score ([(Hearts, Num 2),(Hearts, Jack)],10) = 3
val test10_5 = score ([(Hearts, Num 2),(Clubs, Num 8)],10) = 0

val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6
val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false)
              handle IllegalMove => true)