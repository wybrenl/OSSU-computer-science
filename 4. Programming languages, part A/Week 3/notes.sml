
(* Lecture week 3 notes *)

(*records*)
val x = {bar=(1+2, true andalso true), foo= 3+4, baz=(false,9)};

val my_girlfriend = {birthday=26021993, name="elisabeth"};

#name my_girlfriend;

val a_pair = (3+1, 4+2);

val a_record = {second = 4+2, first = 3+1};

val another_pair = {2=5, 1=6};

#1 a_pair + #2 another_pair;

(*tuples are just a way of writing records -> syntactic sugar*)

(*3 types of bindings: *)
(*-val for values *)
(*-fun for functions *)
(*-datatype for datatypes*)

datatype mytype = TwoInts of int * int
                | Str of string
       | Pizza

             (*mytype -> int*)
fun f (x : mytype) =
    case x of
        Pizza => 3
      | Str s => 8
      | TwoInts(i1,i2) => i1+i2;

(*can acces by*)
f (Pizza);
f (Str "p");
f (TwoInts (3,9));

datatype suit = Club | Diamond| Hear | Spade

datatype rank = Jack | Queen | King | Ace | Num of int

type card = suit * rank

datatype id = StudentNum of int
            | Name of string * (string option) * string;

datatype exp = Constant of int
             | Negate of exp
             | Add of exp * exp
             | Multiply of exp * exp;

fun eval e =
    case e of
        Constant i => i
      | Negate e2 => ~ (eval e2)
      | Add(e1,e2) => (eval e1) + (eval e2)
      | Multiply(e1,e2) => (eval e1) *(eval e2);

val example_exp = Add (Constant 19, Negate (Constant 4))
val example_ans = eval example_exp;

(*v1*)
fun max_constant1 e =
    case e of
        Constant i => i
      | Negate e2 => max_constant1 e2
      | Add(e1,e2) =>
        if max_constant1 e1 > max_constant1 e2
        then max_constant1 e1
        else max_constant1 e2
      | Multiply(e1,e2) =>
        if max_constant1 e1 > max_constant1 e2
        then max_constant1 e1
        else max_constant1 e2

(*v2*)
fun max_constant2 e =
    case e of
        Constant i => i
      | Negate e2 => max_constant2 e2
      | Add(e1,e2) =>
        let
            val m1 = max_constant2 e1
            val m2 = max_constant2 e2
        in if m1 > m2 then m1 else m2 end
      | Multiply(e1,e2) =>
        let
            val m1 = max_constant2 e1
            val m2 = max_constant2 e2
        in if m1 > m2 then m1 else m2 end

(*v3*)
fun max_constant3 e =
    let
        fun max_of_two(e1,e2) =
        let val m1 = max_constant3 e1
            val m2 = max_constant3 e2
        in if m1 > m2 then m1 else m2 end (*Int.max(max_constante1,max_constante2)*)
    in
    case e of
        Constant i => i
      | Negate e2 => max_constant2 e2
      | Add(e1,e2) => max_of_two(e1,e2)
      | Multiply(e1,e2) => max_of_two(e1,e2)
    end

(*v4*)
fun max_constant4 e =
    case e of
        Constant i => i
      | Negate e2 => max_constant4 e2
      | Add(e1,e2) => Int.max(max_constant4 e1, max_constant4 e2)
      | Multiply(e1,e2) => Int.max(max_constant4 e1, max_constant4 e2);


val nineteen1 = max_constant1 example_exp
val nineteen2 = max_constant2 example_exp
val nineteen3 = max_constant3 example_exp
val nineteen4 = max_constant4 example_exp;

fun inc_or_zero intoption =
    case intoption of
        NONE => 0
             | SOME i => i+1

val x1 = inc_or_zero (SOME 0);
val x2 = inc_or_zero NONE;

fun sum_list xs =
    case xs of
        [] => 0
     |  x::xs' => x + sum_list xs'

fun append_list (xs,ys) =
    case xs of
        [] => ys
      | x::xs' => x :: append_list (xs', ys);

val x0 = append_list ([1,2,3,4],[7,6,4,3]);

(*pattern matching each-of*)
fun sum_triple (x,y,z) =
    x + y + z

fun full_name {first = x, middle = y, last = z} =
    x ^ " " ^ y ^ " " ^ z;

(*unexpected polymorphism*)
fun sum_triple (x,y,z) =
    x + z

fun full_name {first = x, middle = y, last = z} =
    x ^ " " ^ z;

exception ListLengthMismatch

(*nested patterns*)
fun zip3 list_triple =
    case list_triple of
        ([],[],[]) => []
      | (hd1::tl1,hd2::tl2,hd3::tl3) =>(hd1,hd2,hd3)::zip3(tl1,tl2,tl3)
      | _ => raise ListLengthMismatch (*_ is for all other cases*)

fun unzip3 lst =
    case lst of
        [] => ([],[],[])
      | (a,b,c)::tl => let val (l1,l2,l3) = unzip3 tl
                       in
                           (a::l1,b::l2,c::l3)
                       end;


fun nondecreasing xs = (* int list -> bool*)
    case xs of
        [] => true
      | _::[] => true
      | head::(neck::rest) => head <= neck andalso nondecreasing (neck::rest);
                  (*
      | x::xs' => case xs' of
                      [] => true
                   | y::ys' => x <= y andalso nondecreasing xs'*)
datatype sgn = P | N | Z

fun multsign (x1,x2) =
    let fun sign x = if x=0 then Z else if x>0 then P else N
    in
        case (sign x1, sign x2) of
            (Z,_) => Z
          | (_,Z) => Z
          | (P,P) => P
          | (N,N) => P
          | _ => N
          (*| (N,P) => N
            | (P,N) => N*)
    end

fun len xs =
    case xs of
        [] => 0
     | _::xs' => 1 + len xs'

(*exceptions*)
fun hd xs =
    case xs of
        [] => raise List.Empty
      | x::_ => x

exception MyUndesirableCondition

exception MyOtherException of int * int

fun mydiv (x,y) =
    case y of
        0 => raise MyUndesirableCondition
      | _ => x div y;

fun maxlist (xs,ex) =
    case xs of
        [] => raise ex
      | x::[] => x
      | x::xs' => Int.max(x,maxlist(xs', ex));

val w = maxlist ([3,4,5], MyUndesirableCondition)
                handle MyUndesirableCondition => 42

val y = maxlist ([], MyUndesirableCondition)
        handle MyUndesirableCondition => 42;

(*tail recursion*)
fun fact n = if n=0 then 1 else n*fact(n-1);
val x = fact 3;

fun factr n=
    let fun aux(n,acc) =
            if n=0
            then acc
            else aux(n-1,acc*n)
    in
        aux(n,1)
    end

val y = factr 4;
