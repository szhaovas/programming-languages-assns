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

fun only_capitals los =
    List.filter (fn s => Char.isUpper (String.sub (s, 0))) los

fun longest_string1 los =
    foldl (fn (s, acc) => if (String.size s) > (String.size acc) then s else acc) "" los

fun longest_string2 los =
    foldl (fn (s, acc) => if (String.size s) >= (String.size acc) then s else acc) "" los

fun longest_string_helper f =
    foldl (fn (s, acc) => if f (String.size s, String.size acc) then s else acc) ""

val longest_string3 = longest_string_helper (fn (x, y) => x > y)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = implode o rev o explode

fun first_answer f list =
    case list of
	[] => raise NoAnswer
      | hd' :: tl' => case f hd' of
			  NONE => first_answer f tl'
			| SOME v => v
					
fun all_answers f list =
    case list of
	[] => SOME []
      | hd' :: tl' => case f hd' of
			  NONE => NONE
			| SOME v => case all_answers f tl' of
					NONE => NONE
				      | SOME w => SOME (v @ w)

val count_wildcards = g (fn _ => 1) (fn _ => 0)

val count_wild_and_variable_lengths = g (fn _ => 1) (fn s => String.size s)

fun count_some_var (str, p) = g (fn _ => 0) (fn s => if str = s then 1 else 0) p

fun check_pat pattern =
    let
	fun get_all_var pat =
	    case pat of
		Variable s => [s] 
	      | TupleP ps => foldl (fn (p, acc) => (get_all_var p) @ acc) [] ps
	      | ConstructorP (_, p) => get_all_var p
	      | _ => []
	
	fun has_repeats los =
	    case los of
		[] => false
	      | hd' :: tl' => List.exists (fn s => hd' = s) tl' orelse has_repeats tl'
    in
	not (has_repeats(get_all_var pattern))
    end

fun match value_pattern =
    case value_pattern of
	(_, Wildcard) => SOME []
      | (v, Variable s) => SOME [(s, v)]
      | (Unit, UnitP) => SOME []
      | (Const int, ConstP i) => if i = int then SOME [] else NONE
      | (Tuple vs, TupleP ps) => if (List.length ps) = (List.length vs)
				 then all_answers match (ListPair.zip (vs, ps))
				 else NONE
      | (Constructor (s2, v), ConstructorP (s1, p)) => if s1 = s2
						       then match (v, p)
						       else NONE
      | _ => NONE

fun first_match value lop =
    SOME (first_answer (fn p => match (value, p)) lop)
    handle NoAnswer => NONE
								
(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* t1 and t2 are typ *)
(* return true when t1 and t2 don't explicitly disagree *)
(* e.g. t1 and t2 explicitly disagree when t1 is UnitT but t2 is IntT, but not when t1 is Anything and t2 is IntT *)
fun compatible_types (t1, t2) =
    if t1 = t2
    then true
    else
	case (t1, t2) of
	    (Anything, _) => true
	  | (_, Anything) => true
	  | (TupleT (h1 :: t1), TupleT (h2 :: t2)) => (if compatible_types (h1, h2)
						       then compatible_types (TupleT t1,TupleT t2)
						       else false)
	  (* the case for Datatype is already covered by t1 = t2 *)
	  | _ => false

fun more_specific_type (t1, t2) =
    if t1 = t2
    then SOME [t1]
    else
	case (t1, t2) of
	    (Anything, _) => SOME [t2]
	  | (_, Anything) => SOME [t1]
	  | (TupleT l1, TupleT l2) => if (List.length l1) = (List.length l2)
				      then (case all_answers more_specific_type (ListPair.zip (l1, l2)) of
						NONE => NONE
					      | SOME types => SOME [TupleT types])
				      else NONE
	  | _ => NONE

(* match a pattern to its most specific typ counterpart *)
(* i.e. a ConstP will get mapped to IntT, not Anything *)
fun matchpt (pattern, types) =
    let
	(* a local function is needed because types need to be saved *)
	fun matchpt_helper (pat, ts) =
	    case pat of
		Wildcard => SOME Anything
	      | Variable s => SOME Anything
	      | UnitP => SOME UnitT
	      | ConstP _ => SOME IntT
	      | TupleP ps => (case ps of
				  [] => SOME (TupleT [])
				| hp :: tp => (case (matchpt_helper (hp, types), matchpt_helper (TupleP tp, types)) of
						   (SOME t, SOME (TupleT list)) => SOME (TupleT (t :: list))
						 | _ => NONE))
	      | ConstructorP (s, p) => (case ts of
					    [] => NONE
					  | (c, dt, t) :: tt => if c = s
								then (case matchpt_helper (p, types) of
									  NONE => matchpt_helper (pat, tt)
									| SOME typ => if compatible_types (typ, t)
										      then SOME (Datatype dt)	   
										      else matchpt_helper (pat, tt))
								else matchpt_helper (pat, tt))
    in
	matchpt_helper (pattern, types)
    end

fun typecheck_patterns (types, lop) =
    case lop of
	[] => NONE
      | hp :: [] => matchpt (hp, types)
      | hp :: tp => (case (matchpt (hp, types), typecheck_patterns (types, tp)) of
			 (NONE, _) => NONE
		       | (_, NONE) => NONE
		       | (SOME match, SOME next_match) => (case more_specific_type (match, next_match) of
							       NONE => NONE
							     | SOME better_match=> SOME (hd better_match)))

