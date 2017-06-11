(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
			       
fun only_capitals strlst =
  List.filter (fn x => Char.isUpper (String.sub (x,0))) strlst
			
fun longest_string1 strlst =
  List.foldl (fn (x, init) => if String.size x > String.size init then x else init) "" strlst

fun longest_string2 strlst =
  List.foldl (fn (x, init) => if String.size x >= String.size init then x else init) "" strlst

fun longest_string_helper f strlst =
  List.foldl (fn (x, init) => if f (String.size x, String.size init) then x else init) "" strlst

val longest_string3 =
    longest_string_helper (fn (x, y) => x > y)

val longest_string4 =
    longest_string_helper (fn (x, y) => x >= y)

val longest_capitalized =
    longest_string1 o only_capitals

val rev_string =
    String.implode o List.rev o String.explode

fun first_answer f lst =
  case lst of
      [] => raise NoAnswer
    | x :: xs' => case f x of
		      SOME v => v
		    | NONE => first_answer f xs'

fun all_answers f lst =
  let fun all_answers_helper (lst1, acc) =
	case lst1 of
	    [] => SOME acc
	  | x :: xs' => case f x of
			    NONE => NONE
			  | SOME v => all_answers_helper (xs', acc@v)
  in all_answers_helper (lst, [])
  end
(*
foldl f init [x1, x2, ..., xn]
returns
f(xn,...,f(x2, f(x1, init))...)
or init if the list is empty. 
*)
