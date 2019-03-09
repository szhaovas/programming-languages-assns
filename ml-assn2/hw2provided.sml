(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* los means list of strings *)
fun all_except_option (str, los) =
    let
	(* los_and_bool contains los and a boolean that indicates whether str has occured in los so far -> true if has occured, false otherwise *)
	fun builder los_and_bool =
	    case los_and_bool of
		([], false) => NONE
	      | ([], true) => SOME []
	      | (hd' :: tl', bool) => if same_string (str, hd')
				      then builder (tl', true)
				      else case builder (tl', bool) of
					       NONE => NONE
					     | SOME list => SOME (hd' :: list)
    in
	builder (los, false)
    end

(* lolos means list of list of strings *)
fun get_substitutions1 (lolos, s) =
    case lolos of
	[] => []
      | hd' :: tl' => case all_except_option (s, hd') of
			NONE => get_substitutions1 (tl', s)
		       | SOME list => list @ get_substitutions1 (tl', s)

fun get_substitutions2 (lolos, s) =
    let
	fun tail_recursor lolos_and_recursor =
	    case lolos_and_recursor of
		([], recursor) => recursor
	      | (hd' :: tl', recursor) => case all_except_option (s, hd') of
					      NONE => tail_recursor (tl', recursor)
					    | SOME list => tail_recursor (tl', recursor @ list)
    in
	tail_recursor (lolos, [])
    end

fun similar_names (substitutions, {first = x, middle = y, last = z}) =
    let
	val first_names = get_substitutions1 (substitutions, x)

	(* builds a list of substitute first names, except for the original name *)
	fun builder first_names =
	    case first_names of
		[] => []
	      | hd' :: tl' => {first = hd', last = z, middle = y} :: builder tl'
    in
	(* cons the original name *)
	{first = x, last= z, middle = y} :: builder first_names
    end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color (suit, rank) =
    case suit of
	Spades => Black
      | Clubs => Black
      | Diamonds => Red
      | Hearts => Red

fun card_value (suit, rank) =
    case rank of
	Num n => n
      | Ace => 11
      | _ => 10

(* cs is a list of cards, c is a card, e is an exception *)
fun remove_card (cs, c, e) =
    case cs of
	[] => raise e
      (* only need to remove the first occurance, so no need to go through the rest of list *)
      | hd' :: tl' => if hd' = c
		      then tl'
		      else hd' :: remove_card (tl', c, e)
			  
fun all_same_color cs =
    case cs of
	[] => true
      | hd' :: [] => true
      | hd1 :: hd2 :: tl' => (card_color hd1) = (card_color hd2)
			     andalso all_same_color (hd2 :: tl')

fun sum_cards cs =
    let
	fun tail_recursor (cs, recursor) =
	    case cs of
		[] => recursor
	      | hd' :: tl' => tail_recursor (tl', (card_value hd') + recursor)
    in
	tail_recursor (cs, 0)
    end

fun score (held_cards, goal) =
    let
	val sum = sum_cards held_cards
	val same_color = all_same_color held_cards
    in
	if sum > goal
	then
	    if same_color
	    then 3 * (sum - goal) div 2
	    else 3 * (sum - goal)
	else
	    if same_color
	    then (goal - sum) div 2
	    else goal - sum
    end

fun officiate (card_list, move_list, goal) =
    let
	fun recursor states =
	    case states of
		([], _, held_cards, _, _) => score (held_cards, goal)
	      | (_, [], held_cards, _, _) => score (held_cards, goal)
	      | (_, _, held_cards, _, true) => score (held_cards, goal)
	      | (nextc :: restc, nextm :: restm, held_cards, sum, above_goal) =>
		case nextm of
		    Draw => recursor (restc, restm, nextc :: held_cards, sum + (card_value nextc), (sum + (card_value nextc)) > goal)
		  | Discard c => recursor (nextc :: restc, restm, remove_card (held_cards, c, IllegalMove), sum - (card_value nextc), above_goal)
    in
	recursor (card_list, move_list, [], 0, false)
    end
(*
(* challenge problems start here *)
(* return a list of possible sums, including the original sum *)
fun possible_sums cs =
    let
	(* sum is the original sum, with all Ace having values of 11 *)
	fun builder (cs, sum) =
	    case cs of
		[] => []
	      (* Ace can either be 1 or 11, which makes a difference of 10 *)
	      (* so subtracting 10 from the original sum is the same as changing one Ace value from 11 to 1 *)
	      | (_, rank) :: tl' => if rank = Ace
				    then (sum - 10) :: builder (tl', sum - 10)
				    else builder (tl', sum)

	val sum = sum_cards cs
    in
	sum :: builder (cs, sum)
    end
*)

fun score_challenge (held_cards, goal) =
    let
	(* this helper function returns true if the preliminary score of sum2 is less than sum1 *)
	fun closer_to_goal (sum1, sum2) =
	    let
		val diff1 = sum1 - goal
		val diff2 = sum2 - goal
	    in
		if (diff1 <= 0) = (diff2 <= 0)
		then diff1 > diff2
		else if diff1 <= 0 andalso diff2 > 0
		then (~diff1) > (diff2 * 3)
		else (diff1 * 3) > (~diff2)
	    end
		
	fun best_sum (held_cards, prelim, prev_prelim) =
	    if prelim <= goal
	    then if closer_to_goal (prelim, prev_prelim)
		 then prev_prelim
		 else prelim
	    else
		case held_cards of
		    [] => if closer_to_goal (prelim, prev_prelim)
			  then prev_prelim
			  else prelim
		  | (_, rank) :: tl' => if rank = Ace
					then best_sum (tl', prelim - 10, prelim)
					else best_sum (tl', prelim, prev_prelim)
	val sum = sum_cards held_cards
	val best_prelim = best_sum (held_cards, sum, sum)
	val same_color = all_same_color held_cards
    in
	if best_prelim > goal
	then
	    if same_color
	    then 3 * (best_prelim - goal) div 2
	    else 3 * (best_prelim - goal)
	else
	    if same_color
	    then (goal - best_prelim) div 2
	    else goal - best_prelim
    end
	
(*			    
fun score_challenge (held_cards, goal) =
    let
	val candidates = possible_sums held_cards
	val same_color = all_same_color held_cards

	(* this one takes input from possible_sums, and return the sum that gives the best preliminary score *)
	fun best_sum candidates =
	    case candidates of
		hd' :: [] => hd'
	      | hd' :: tl' =>
		let
		    val min = best_sum tl'
		in
		    if closer_to_goal (hd', min)
		    then min
		    else hd'
		end

	(* this one calculates the score based on card value sum and a boolean indicating whether all cards have the same color *)
	fun score_by_sum (sum, same_color) =
	    if sum > goal
	    then
		if same_color
		then 3 * (sum - goal) div 2
		else 3 * (sum - goal)
	    else
		if same_color
		then (goal - sum) div 2
		else goal - sum
    in
	score_by_sum (best_sum candidates, same_color)
    end
*)

fun officiate_challenge (card_list, move_list, goal) =
    let
	fun new_card_value (_, rank) =
	    case rank of
		Num n => n
	      | Ace => 1
	      | _ => 10
	
	fun recursor (card_list, move_list, held_cards, lowest_sum) =
	    if lowest_sum > goal
	    then score_challenge (held_cards, goal)
	    else
		case (card_list, move_list) of
		    ([], _) => score_challenge (held_cards, goal)
		  | (_, []) => score_challenge (held_cards, goal)
		  | (nextc :: restc, nextm :: restm) =>
		    case nextm of
			Draw => recursor (restc, restm, nextc :: held_cards, lowest_sum + (new_card_value nextc))
		      | Discard c => recursor (nextc :: restc, restm, remove_card (held_cards, c, IllegalMove), lowest_sum - (new_card_value c))
    in
	recursor (card_list, move_list, [], 0)
    end  

(*
fun officiate_challenge (card_list, move_list, goal) =
    let
	fun any_exceed_goal sums =
	    case sums of
		[] => false
	      | hd' :: tl' => if hd' > goal
			      then true
			      else any_exceed_goal tl'
	
	fun recursor states =
	    case states of
		([], _, held_cards, _) => score_challenge (held_cards, goal)
	      | (_, [], held_cards, _) => score_challenge (held_cards, goal)
	      | (_, _, held_cards, true) => score_challenge (held_cards, goal)
	      | (nextc :: restc, nextm :: restm, held_cards, above_goal) =>
		case nextm of
		    Draw => recursor (restc, restm, nextc :: held_cards, any_exceed_goal (possible_sums (nextc :: held_cards)))
		  (* state of above_goal cannot possibly change after removing a card, given that the game is not over *)
		  | Discard c => recursor (nextc :: restc, restm, remove_card (held_cards, c, IllegalMove), above_goal)
    in
	recursor (card_list, move_list, [], false)
    end
*)

fun careful_player (card_list, goal) =
    let
	(* this one checks if any card in held_cards has value equal to value *)
	(* return NONE if no such card exists *)
	(* return SOME card if card meets the requirement *)
	fun check_equal (held_cards, value) =
	    case held_cards of
		[] => NONE
	      | hd' :: tl' => if (card_value hd') = value
			      then SOME hd'
			      else check_equal (tl', value)

	fun player (card_list, held_cards, card_sum) =
	    case card_list of
		[] => []
	      | nextc :: restc => 
		if card_sum = goal
		then []
		else if goal - card_sum > 10
		then Draw :: player (restc, nextc :: held_cards, card_sum + (card_value nextc))
				    (* Q: What if it is possible to reach goal by simply drawing without discarding one of the cards? *)
		else case check_equal (held_cards, card_sum + (card_value nextc) - goal) of
			 NONE => []
		       | SOME card => (Discard card) :: Draw :: []
    in
	player (card_list, [], 0)
    end