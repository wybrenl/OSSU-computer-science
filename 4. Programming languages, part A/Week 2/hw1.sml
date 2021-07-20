(*HW week 2*)

(*Q1*)
(* int*int*int int*int*int -> bool*)
(* returns true if date 1 is before date 2, false otherwise*)

fun is_older(date1 : int*int*int, date2 : int*int*int) =
    if (#1 date1) < (#1 date2) then true (* check if year 1 is before year 2*)
    else if (#1 date1) > (#1 date2) then false (* check year 1 is after year 2*)
    else if (#2 date1) < (#2 date2) then true (* years are equal, check if month1 is before month 2*)
    else if (#2 date1) > (#2 date2) then false (*year are equal, check if month1 is after month2*)
    else if (#3 date1) < (#3 date2) then true (*equals months and years, check for day*)
    else false;

(*Q2*)
(* datelist int -> int *)
(* returns number of date in a given month*)

fun number_in_month (datelist: (int*int*int) list, month: int) = 
    if null datelist
    then 0
    else    if #2 (hd datelist) = month
            then 1 + number_in_month(tl datelist, month)
            else number_in_month(tl datelist, month)

(*Q3*)
(* (int*int*int) list int list -> int*)
(* returns number of dates in a set of months*)

fun number_in_months (datelist: (int*int*int) list, months: int list) = 
    if null months
    then 0
    else number_in_month(datelist, hd months) + number_in_months(datelist, tl months)

(*Q4*)
(* (int*int*int) list  int -> (int*int*int) list*)
(* returns a list of dates that are in a given month*)

fun dates_in_month (datelist : (int*int*int) list, month : int) =
    if null datelist
    then []
    else if month = #2 (hd datelist) then (hd datelist) :: dates_in_month (tl datelist, month) else dates_in_month (tl datelist, month)

(*Q5*)
(* (int*int*int) list int list -> (int*int*int) list *)
(* returns a list of dates that are in a set of months*)

fun dates_in_months (datelist : (int*int*int) list, months : int list) =
    if null months
    then []
    else dates_in_month(datelist, (hd months)) @ dates_in_months(datelist, (tl months))

(*Q6*)
(* string list int -> string*)
(* returns the nth element of a list of strings*)

fun get_nth (stringlist : string list, n : int) =
    if n=1
    then hd stringlist
    else get_nth(tl stringlist, n-1)

(*Q7*)
(* int*int*int -> string *)
(* returns a stringdate based on a date*)

fun date_to_string (date : int*int*int) =
    let 
        val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in 
        get_nth (months, #2 date)^" "^ Int.toString(#3 date)^ ", "^Int.toString(#1 date)
    end

(*Q8*)
(* int list int -> int *)
(* returns the n number of elements from a list that added together are below sum, n+1 element would be sum or above*)

fun number_before_reaching_sum (sum : int, numbers : int list) =
    let
        fun helper (sum : int, numbers : int list, count : int) =
            if (sum - (hd numbers)) <= 0
            then count-1
            else helper (sum - hd numbers, tl numbers, count +1)
    in 
        helper (sum, numbers, 1)
    end

(*Q9*)
(* int -> string*)
(* returns month for a given day of year*)

fun what_month (day : int) =
    let 
        val monthdays = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in 
        number_before_reaching_sum (day, monthdays)+1
    end

(*Q10*)
(* int int -> int list*)
(* returns the months of the days between two days in the year*)

fun month_range (day1 : int, day2 : int) = 
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1+1, day2)

(*Q11*)
(* (int*int*int) list -> (int*int*int) option *)
(* returns none if there are no dates, otherwise it produces an option of the oldest date *)

fun oldest (datelist : (int*int*int) list) = 
    if null datelist
    then NONE
    else
        let 
            val tl_ans = oldest (tl datelist)
        in
            if isSome tl_ans andalso is_older(valOf tl_ans, hd datelist)
            then tl_ans
            else SOME(hd datelist)
        end

fun remove_duplicates (numbers : int list ) =
                if null numbers 
                then []
                else hd(numbers) :: remove(hd numbers, remove_duplicates(tl numbers))3w