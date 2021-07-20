(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)
use "hw1.sml";

val date1 = (1990, 12, 29);
val date2 = (2000, 12, 20);
val date3 = (1989, 11, 13);

val test1_1 = is_older(date1, date2) = true;
val test1_2 = is_older(date1, date3) = false;
val test1_3 = is_older(date1, date1) = false;

val test2_1 = number_in_month ([date1, date2, date3], 12) = 2
val test2_2 = number_in_month ([date1, date2, date3], 11) = 1
val test2_3 = number_in_month ([date1, date2, date3], 10) = 0

val test3_1 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3
val test3_2 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2])     = 1
val test3_3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[9])     = 0
val test3_4 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[])      = 0


val test4_1 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
val test4_2 = dates_in_month ([(2012,2,28),(2013,2,2)], 2) = [(2012,2,28),(2013,2,2)]
val test4_3 = dates_in_month ([(2012,2,28),(2013,12,1)],3) = []
val test4_4 = dates_in_month ([],2) = []


val test5_1 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]
val test5_2 = dates_in_months ([(2012,3,28),(2013,12,1),(2011,3,31),(2011,3,28)],[3])     = [(2012,3,28),(2011,3,31),(2011,3,28)]
val test5_3 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[11])    = []
val test5_4 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[])      = []

val test6_1 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"
val test6_2 = get_nth (["hi", "there", "how", "are", "you"], 3) = "how"
val test6_3 = get_nth (["hi", "there", "how", "are", "you"], 5) = "you"


val test7_1 = date_to_string (2013, 6, 1) = "June 1, 2013"
val test7_2 = date_to_string (1990, 12, 29) = "December 29, 1990"

val test8_1 = number_before_reaching_sum (0, [1,2,3,4,5]) = 0
val test8_2 = number_before_reaching_sum (5, [1,2,3,4,5]) = 2
val test8_3 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3


val test9_1 = what_month 70 = 3

val test10_1 = month_range (31, 34) = [1,2,2,2]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)

