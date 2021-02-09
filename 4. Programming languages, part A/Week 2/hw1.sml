(* Homework PLA week 2*)

(*Q1*)
(* int*int*int int*int*int -> boolean*)
(* produce true if first date is before the second, if they are the same return false*)
fun is_older(date1 : int*int*int, date2 : int*int*int) =
    if (#1 date1) < (#1 date2) then true (* check if year 1 is before year 2*)
    else if (#1 date1) > (#1 date2) then false (* check year 1 is after year 2*)
    else if (#2 date1) < (#2 date2) then true (* years are equal, check if month1 is before month 2*)
    else if (#2 date1) > (#2 date2) then false (*year are equal, check if month1 is after month2*)
    else if (#3 date1) < (#3 date2) then true (*equals months and years, check for day*)
    else false;


(*Q2*)
(* (int*int*int) list * int -> int *)
(* produces how many dates in the list are in the given month *)
fun number_in_month (datelist : (int*int*int) list, month : int) =
    if null datelist     (*check if list is empty*)
    then 0               (*add 0 if empty*)
    else if (#2 (hd datelist)) = month
    then 1 + number_in_month((tl datelist), month) (*recursive call on function adding 1 for each time the date is in the given month, 0 otherwise *)
    else number_in_month((tl datelist), month);

(*Q3*)
(* (int*int*int) list * int list) -> int *)
(* produces how many dates in the list are in the given months in the list*)
fun number_in_months (datelist : (int*int*int) list, months : int list) =
    if null months
    then 0
    else number_in_month (datelist, (hd months)) + number_in_months (datelist, (tl months)); (*count occurences for first month, then do the same for other months in the list*)

(*Q4*)
(* (int*int*int) list * int -> (int*int*int) list *)
(* produces a list of dates that are in a given month based on an existing list of dates*)
fun dates_in_month (datelist : (int*int*int) list, month : int) =
    if null datelist
    then []
    else if (#2 (hd datelist)) = month then (hd datelist) :: dates_in_month ((tl datelist), month)
    else dates_in_month ((tl datelist), month);

(*Q5*)
(* (int*int*int) list * int list -> (int*int*int) list) *)
(* produces a list of dates that are in a list of months based on an existing list of dates*)
fun dates_in_months (datelist : (int*int*int) list, months : int list) =
    if null months
    then []
    else dates_in_month (datelist, (hd months)) @ dates_in_months (datelist, (tl months));

(*Q6*)
(*string list * int -> string*)
(*produces the nth element in a list of strings*)
fun get_nth (stringlist : string list, n : int) =
    if n = 1
    then (hd stringlist)
    else get_nth (tl stringlist, n-1);

(*7 *)
(* int*int*int -> string*)
(*produces a date string if the form of Month Day, Year e.g. 20 January 2020*)
fun date_to_string (date : int*int*int) =
    let
        val monthlist = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
        get_nth (monthlist, (#2 date))^" "^Int.toString (#3 date)^", "^Int.toString(#1 date)
    end

(*Q8 *)
(* int * int list -> int *)
(* produces nth elements such that the sum of n elements is less than the given number *)
fun number_before_reaching_sum (number : int, numlist : int list) =
     let fun number_before_reaching_sum2help (numlist : int list, countdown : int, count : int) =
            if countdown - (hd numlist) > 0
            then number_before_reaching_sum2help (tl numlist, (countdown - hd numlist), count + 1)
            else count
    in
        number_before_reaching_sum2help (numlist, number, 0)
    end

(*Q9 *)
(* int -> int *)
(* produces the month for a given day in the year between 1 to 365 *)
fun what_month (date : int) =
    let
        val days_per_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        number_before_reaching_sum (date, days_per_month) + 1
    end

(*Q10 *)
(* int * int -> int list *)
(* produces the month number for all days in between two day numbers*)
fun month_range (day1 : int, day2 : int) =
    let fun make_list (from : int, to : int) = (* make list of numbers between 2 days including both days*)
            if from > to then []               (* return empty list if day1 is after day2*)
            else if from = to then to::[]
            else from :: make_list(from+1, to)
        fun what_month_list (datelist : int list) = (* assign months to each day *)
            if null datelist
            then []
            else what_month (hd datelist) :: what_month_list (tl datelist)
        val month_range_list = make_list (day1, day2) (* creat list for days specified in top fucntion*)
    in
       what_month_list (month_range_list)
    end

(*Q11 *)
(* int*int*int list -> int*int*int) *)
(* returns the oldest date in a list of dates*)
fun oldest(listofdates : (int*int*int) list) =
    if null listofdates then NONE (* return NONE if there is an empty list*)
    else
        let val tl_oldest = oldest(tl listofdates) (*oldest date in the tail*)
        in
            if isSome tl_oldest andalso is_older(valOf tl_oldest, (hd listofdates)) (*compares head of list vs. tail of list*)
            then tl_oldest
            else SOME(hd listofdates)
        end
