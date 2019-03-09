fun is_older (date1 : int*int*int, date2: int*int*int) =
    if (#1 date1) <> (#1 date2)
    then (#1 date1) < (#1 date2)
    else if (#2 date1) <> (#2 date2)
    then (#2 date1) < (#2 date2)
    else if (#3 date1) <> (#3 date2)
    then (#3 date1) < (#3 date2)
    else false


fun number_in_month (lod : (int*int*int) list, month : int) =
    let
	fun counter (lod : (int*int*int) list) =
	    if null lod
	    then 0
	    else if (#2 (hd lod)) = month
	    then 1 + counter (tl lod)
	    else counter (tl lod)
    in
	counter (lod)
    end


fun number_in_months (lod : (int*int*int) list, months : int list) =
    let
	fun counter (months : int list) =
	    if null months
	    then 0
	    else number_in_month (lod, hd months) + counter (tl months)
    in
	counter (months)
    end


fun dates_in_month (lod : (int*int*int) list, month : int) =
    let
	fun appender (lod : (int*int*int) list) =
	    if null lod
	    then []
	    else if (#2 (hd lod)) = month
	    then (hd lod) :: appender (tl lod)
	    else appender (tl lod)
    in
	appender (lod)
    end

	
fun dates_in_months (lod : (int*int*int) list, months : int list) =
    let
	fun appender (months : int list) =
	    if null months
	    then []
	    else dates_in_month (lod, hd months) @ appender (tl months)
    in
	appender (months)
    end


fun get_nth (los : string list, nth : int) =
    let
	fun wrapper (los : string list, nth : int) =
	    if nth <= 1
	    then los
	    else wrapper (tl los, nth - 1)
    in
	hd (wrapper (los, nth))
    end


fun get_nth_int (loi : int list, nth : int) =
    let
	fun wrapper (loi : int list, nth : int) =
	    if nth <= 1
	    then loi
	    else wrapper (tl loi, nth - 1)
    in
	hd (wrapper (loi, nth))
    end


fun date_to_string (date : int*int*int) =
    let
	val months = ["January ", "February ", "March ", "April ", "May ", "June ", "July ", "August ", "September ", "October ", "November ", "December "]
    in
	get_nth (months, #2 date) ^ Int.toString (#3 date) ^ ", " ^ Int.toString (#1 date)
    end


fun number_before_reaching_sum (sum : int, loi : int list) =
    let
	fun helper (counter : int, loi : int list) =
	    if counter >= sum
	    then 0
	    else 1 + (helper (counter + (hd loi), tl loi))
    in
	if null (tl loi)
	then hd loi
	else helper (hd loi, tl loi)
    end
	

fun what_month (doy : int) =
    let
	val dom = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	number_before_reaching_sum (doy, dom) + 1
    end


fun month_range (day1 : int, day2 : int) =
    let
	fun helper (day1 : int) =
	    if day1 > day2
	    then []
	    else (what_month day1) :: helper (day1 + 1)
    in
	helper (day1)
    end


fun oldest (lod : (int*int*int) list) =
    if null lod
    then NONE
    else 
	let
	    fun oldest_nonempty (lod : (int*int*int) list) =
		if null (tl lod)
		then hd lod
		else
		    let
			val oldest_candidate = oldest_nonempty (tl lod)
		    in
			if is_older (hd lod, oldest_candidate)
			then hd lod
			else oldest_candidate
		    end
	in
	    SOME (oldest_nonempty lod)
	end


fun remove_duplicates (loi : int list) =
    let
	(* this helper function returns true if loi contains an integer equal to num *)
	fun has_equal (loi : int list, num : int) =
	    if null loi
	    then false
	    else if (hd loi) = num
	    then true
	    else has_equal (tl loi, num)

	(* this helper function builds the result by appending a member of loi to the result list only if it doesn't equal to any member of the result list *)
	fun result_builder (loi : int list, result : int list) =
	    if null loi
	    then result
	    else if has_equal (result, hd loi)
	    then result_builder (tl loi, result)
	    else result_builder (tl loi, (hd loi) :: result)

	fun reverse (loi : int list, result : int list) =
	    if null loi
	    then result
	    else reverse (tl loi, (hd loi) :: result)
    in
	reverse (result_builder (loi, []), [])
    end
	
	
fun number_in_months_challenge (lod : (int*int*int) list, months : int list) =
    let
	val unique_months = remove_duplicates months
    in
	number_in_months (lod, unique_months)
    end


fun dates_in_months_challenge (lod : (int*int*int) list, months : int list) =
    let
	val unique_months = remove_duplicates months
    in
	dates_in_months (lod, unique_months)
    end


fun reasonable_date (date : int*int*int) =
    let
	fun is_leap (date : int*int*int) =
	    if ((#1 date) mod 400) = 0
	    then true
	    else if ((#1 date) mod 4) <> 0
	    then false
	    else if ((#1 date) mod 100) <> 0
	    then true
	    else false

	val dom = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

	(* for readibility, not good style *)
	val dom_leap = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

	fun checker (date : int*int*int, dom : int list) =
	    if (#1 date) <= 0
	    then false
	    else if (#2 date) < 1 orelse (#2 date) > 12
	    then false
	    else if (#3 date) < 1 orelse (#3 date) > get_nth_int (dom, #2 date)
	    then false
	    else true
    in
	if is_leap date
	then checker (date, dom_leap)
	else checker (date, dom)
    end
