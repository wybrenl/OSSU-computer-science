(*Lecture notes week 4 *)
(*examples first class function*)
fun double x = 2 * x;
fun incr x = x + 1;
val a_tuple = (double, incr, double(incr 7));
val eighteen = (#1 a_tuple) 9;

(*functions as arguments*)
fun n_times (f,n,x) =
    if n=0
    then x
    else f (n_times(f,n-1,x));

fun increment x = x +1
fun double x = x + x;

val test = n_times (increment, 4, 1);

fun addition (n,x) = n_times (increment,n,x)
fun double_n (n,x) = n_times(double,n,x)
fun n_thtail (n,x) = n_times(tl, n,x);

(*anonymous functions*)
fun triple_n_times (n,x) =
    let
        fun triple x = 3* x
    in
        n_times(triple,n,x)

fun triple_n_times_let_lame (n,x) =
    n_times (let fun triple x = x * 3 in triple end,n,x)

fun triple_n_times_anonymous (n,x) =
    n_times ((fn x => 3*x),n,x) (*fn not fun for anonymous functions*);

(*poor style use of anonymous*)
fun rev xs = List.rev xs (*poor style*)

val rev = fn xs => List.rev xs (*poorest style*)

val rev = List.rev (*superior style*);

(*map and filter*)
fun map (f, xs) = (*f applied to each element*)
    case xs of
        [] =>[]
      | x::xs' => (f x)::map(f,xs');

val x1 = map((fn x => x+1),[4,8,12,16])
val x2 = map(hd, [[1,2],[3,4],[5,6,7]])

fun filter(f,xs) =
    case xs of
        [] => []
      | x::xs' => if f x
                  then x::(filter (f,xs'))
                  else filter(f,xs');

fun is_even v =
    (v mod 2 = 0)

fun all_even xs =
    filter(is_even,xs);

fun all_even_snd xs =
    filter((fn (_,v) => is_even v), xs)

(*return a function*)

fun double_or_triple f =
    if f 7
    then fn x => 2*x
    else fn x => 3*x;

val double = double_or_triple (fn x => x- 3 = 4);
val nine = (double_or_triple (fn x => x = 42)) 3;
val six = (double_or_triple (fn x => x = 7)) 3;

datatype exp = Constant of int
             | Negate of exp
             | Add of exp * exp
             | Multiply of exp * exp;


(*generalizing*)
(*given an exp, is every constant in it an even number? or <10 or whatever*)
(*higher-order predicate*)
fun true_of_all_constants(f,e) =
    case e of
        Constant i => f i
      | Negate e1 => true_of_all_constants(f,e1)
      | Add(e1,e2) => true_of_all_constants(f,e1) andalso true_of_all_constants(f,e2)
      | Multiply(e1,e2) => true_of_all_constants(f,e1) andalso true_of_all_constants(f,e2);

fun all_even e = true_of_all_constants((fn e => is_even e), e);

(*lexical scope*)

(*1 *) val x = 1 (*x maps to 1*)
(*2 *) fun f y = x + y (*f maps to a function that adds 1 to its argument*)
(*3 *) val x = 2 (*x maps to 2*)
(*4 *) val y = 3 (*y maps to 3*)
(*5 *) val z = f (x+y) (*call the function defined on line 2 with 5 (2 + 3) = 6*)
(*crux is that in line 5 x + y is evaluated with line 3 and 4, while f y is being evaluated with + 1 *)
(*from the old environment*);

(*closures*)
val x = 1
fun f y =
    let
        val x = y +1
    in
        fn z => x + y + z (*zak z and return 2y+1+z*)
    end;

val x = 3 (*irrelevant*)
val g = f 4 (*return a functions that adds 9 to its argument*)
val y = 5
val z = g 6 (*get 15 because 9 + 6*)
val z2 = f 4;

fun f g =
    let
        val x = 3 (*irrelevant*)
    in
        g 2
    end;

val x = 4
fun h y = x + y (*add 4 to its argument*)
val z = f h (*6*);

(*closures and recomputation*)
fun filter(f,xs) =
    case xs of
        [] => []
      | x::xs' => if f x
                  then x::(filter (f,xs'))
                  else filter(f,xs');

fun allshorterthan1 (xs, s) = (*string list * string -> string list*)
    filter (fn x => String.size x < (print "!" ; String.size s), xs);
(*recomputes string.size s every time*)

fun allshorterthan2 (xs, s) =
    let
        val i = (print "!" ; String.size s)
    in
        filter(fn x => String.size x < i, xs)
    end;

(*print*)
val _ = print "\nwithallshorterthan1: ";

val x1 = allshorterthan1 (["1", "333", "22", "4444"], "xxx");

val _ = print "\nwithallshorterthan2: ";

val x2 = allshorterthan2 (["1", "333", "22", "4444"], "xxx");

(*fold*)
fun fold (f, acc, xs) =
    case xs of
        [] => acc
      | x::xs' => fold (f, f(acc,x), xs');

fun f1 xs = fold ((fn (x,y) => x+y), 0, xs) (*sum list*);
fun f2 xs = fold ((fn (x,y) => x andalso y>=0), true, xs) (*are all list elements non-negative*);
(*count # of elements between low and high*)
fun f3 (xs, lo, hi) =
    fold ((fn (x,y) =>
              x + (if y >= lo andalso y <= hi then 1 else 0)),
          0, xs);
(* check if all elements are less than length i*)
fun f4 (xs,s) =
    let
        val i = String.size s
    in
        fold ((fn (x,y) => x andalso String.size y < i), true, xs)
    end;
(* do all elements of the list produce true when passed to g*)
fun f5 (g, xs) = fold((fn(x,y) => x andalso g y), true, xs);
fun f4again (xs,s) =
    let
        val i = String.size s
    in
        f5(fn y => String.size y < i, xs)
    end

(*closure idiom: *)
(* - pass functions with private data to iterators: see above *)
(* - combing functions *)
(* - currying *)
(* - callbacks *)
(* - implementing an ADT with a record of functions*);

(*composition*)
fun compose (f,g) = fn x => f(g x)
(* ('b -> 'c) * ('a -> 'b) -> ('a -> 'c)*);
(*f o g*) (*same this as above*);

fun sqrt_of_abs1 i = Math.sqrt (Real.fromInt (abs i));
fun sqrt_of_abs2 i = (Math.sqrt o Real.fromInt o abs) i;
val sqrt_of_abs3 = Math.sqrt o Real.fromInt o abs;

(* |> !> *)
infix !>

fun x !> f = f x;

fun sqrt_of_abs i = i !> abs !> Real.fromInt !> Math.sqrt

fun backup1 (f,g) =
    fn x => case f x of
                NONE => g x
              | SOME y => y;

(*currying*)
fun sorted3_tupled (x,y,z) = z >= y andalso y >= x
val t1 = sorted3_tupled (7,9,11);

val sorted3 = fn x => fn y => fn z => z >= y andalso y >= x;
(*fun sorted3 x = fn y => fnz => ...*)
val t2 = ((sorted3 7) 9) 11
val t3 = sorted3 7 9 11;

fun sorted3_nicer x y z = z >= y andalso y>= x;
val t4 = sorted3_nicer 7 9 11;

fun fold f acc xs =
    case xs of
        [] => []
      | x::xs' => fold f (f(acc,x)) xs';

fun sum xs = fold (fn (x,y) => x+y) 0 xs;

(*partial application*)
(*entering too few arguments to a curried function -> can be useful*)
val is_nonnegative = sorted3 0 0
fun is_nonnegative_inferior x = sorted3 0 0 x;
val sum = fold (fn (x,y) => x + y) 0;
fun sum_inferior xs = fold (fn (x,y) => x + y) 0 xs

fun range i j = if i > j then [] else i::range(i+1) j;
val countup = range 1;
val x = countup 6

fun exists predicate xs =
    case xs of
        [] => false
      | x::xs' => predicate x orelse exists predicate xs';

val no = exists (fn x => x=7) [4,11,23];
val haszero = exists (fn x => x=0);
val incrementall = List.map (fn x => x + 1)
val removezeros = List.filter (fn x => x <> 0);

val pairwithone = List.map (fn x => (x,1));

fun curry f x y = f (x,y);
fun uncurry f (x,y) = f x y;
fun other_curry f x y = f y x;

(*http://www.standardml.org/Basics/manpages.html*)
(*list structure link*)
