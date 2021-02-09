(* This is a comment. This is our first program. *)


val x = 34; (* int *)
(* dynamic environment: x --> 34 *)

(* val = declare a variable; x is name of variable) *)

val y = 17;
(* dynamic environment: y --> 17 *)

val z = (x + y) + (y + 2);

val abs_of_z = if z < 0 then 0 - z else z;

val abs_of_z_simpler = abs z;
