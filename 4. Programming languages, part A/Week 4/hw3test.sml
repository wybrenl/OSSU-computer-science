(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw33.sml";

val test1_1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test1_2 = only_capitals ["A","B","C","d"] = ["A","B","C"]
val test1_3 = only_capitals ["a","b","c"] = []

val test2_1 = longest_string1 ["A","bc","C"] = "bc"
val test2_2 = longest_string1 [] = ""
val test2_3 = longest_string1 ["A","bc","cC"] = "bc"

val test3_1 = longest_string2 ["A","bc","C"] = "bc"
val test3_2 = longest_string2 ["A","bc","CC"] = "CC"
val test3_3 = longest_string2 [] = ""

val test4a_1 = longest_string3 ["A","bc","C"] = "bc"
val test4a_2 = longest_string1 [] = ""
val test4a_3 = longest_string1 ["A","bc","cC"] = "bc"

val test4b_1 = longest_string4 ["A","B","C"] = "C"
val test4b_2 = longest_string2 ["A","bc","CC"] = "CC"
val test4b_3 = longest_string2 [] = ""

val test5_1 = longest_capitalized ["A","bc","C"] = "A"
val test5_2 = longest_capitalized ["A","bc","Cc"] = "Cc"
val test5_3 = longest_capitalized [] = ""

val test6_1 = rev_string "abc" = "cba"
val test6_2 = rev_string "" = ""
val test6_3 = rev_string "aBcDe" = "eDcBa";

val test7_1 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val test7_2 = ((first_answer (fn x => if x > 5 then SOME x else NONE) [1,2,3,4,5]); false) handle NoAnswer => true

val test8_1 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [1,3,4,5,6,7] = NONE
val test8_2 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [1] = SOME [1]
val test8_3 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [1, 1, 1] = SOME[1,1,1]
val test8_4 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [] = SOME[]

val test9a_1 = count_wildcards Wildcard = 1
val test9a_2 = count_wildcards (Variable "string") = 0

val test9b_1 = count_wild_and_variable_lengths (Variable("a")) = 1
val test9b_2 = count_wild_and_variable_lengths (Variable("ab")) = 2

val test9c = count_some_var ("x", Variable("x")) = 1
(*
val test10 = check_pat (Variable("x")) = true

val test11 = match (Const(1), UnitP) = NONE

val test12 = first_match Unit [UnitP] = SOME []
*)