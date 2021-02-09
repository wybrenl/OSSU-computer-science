fun countdown (x : int) =
    if x = 0
    then []
    else x :: countdown(x-1)

(* int list -> int *)
(* produces the sum of the integers in a list) *)
(* sum_list 3 = 6 *)
(* sum_list 4 = 10 *)
fun sum_list (x : int list) =
    if null x
    then 0
    else hd x + sum_list(tl x);

(* int list -> int *)
(* adds a list of int with alternating signs + - *)
(* alternate [1,2,3,4] = 1 - 2 + 3 - 4 = -2 *)

fun alternate (x : int list) =
    if null x
    then 0
    else hd x - hd (tl x) + alternate(tl (tl x));

fun silly1 (z: int) =
    let
        val x = if z > 0 then z else 34
        val y = x + z + 9
    in
        if x > y then x * 2 else y * y
    end;

fun silly2 () =
    let
        val x = 1
    in
        (let val x = 2 in x+1 end) + (let val y = x+2 in y+1 end)
    end;
