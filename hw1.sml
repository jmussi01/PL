fun is_older((y1:int, m1:int, d1:int),(y2:int, m2:int, d2:int)) =
    if y1<y2
    then true
    else (
    	 if y1=y2 andalso m1<m2
	 then true
	 else (
	      if y1=y2 andalso m1=m2 andalso d1<d2
	      then true
	      else false))

fun number_in_month (dates : (int*int*int) list, month : int) =
    if dates = nil
    then 0
    else
	(if #2 (hd dates) = month then 1 else 0) + number_in_month (tl dates,month)


fun number_in_months (dates:(int*int*int)list, months:int list)=
    if months = nil
    then 0
    else
	number_in_month(dates, hd months) + number_in_months(dates, tl months)


fun dates_in_month(dates:(int*int*int)list, month:int)=
    if dates = nil
    then []
    else (if #2(hd dates)=month
    	 then (hd dates)::dates_in_month(tl dates, month)
	 else dates_in_month(tl dates, month))

fun dates_in_months(dates:(int*int*int)list, months:int list)=
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)


fun get_nth(list:string list, n:int)=
    if list = nil
    then ""
    else (if n = 1
    	 then hd list
	 else get_nth(tl list, n-1))


fun date_to_string(date:(int*int*int))=
    let val month = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
    get_nth(month, #2(date))^" "^Int.toString(#3(date))^", "^Int.toString(#1(date))
    end


fun number_before_reaching_sum (sum : int, is : int list) =
    let
        fun helper (is : int list, acc : int, n : int) =
            if acc >= sum then
                n - 1 
            else
                helper(tl(is), acc + hd(is), n + 1)
    in
       helper(is, 0, 0)
    end


fun what_month (day : int) =
  let
      val day_of_each_month = [31, 28, 31, 30,
			       31, 30, 31, 31,
			       30, 31, 30, 31] 
      fun count_days (rest_days : int, day_of_each_month : int list, nth : int) =
	if rest_days <=  hd day_of_each_month
	then nth
	else count_days(rest_days - hd day_of_each_month, tl day_of_each_month, nth + 1)
  in
      count_days(day, day_of_each_month, 1)
  end


fun month_range (day1 : int, day2 : int) =
  if day1 > day2
  then []
  else if day1 = day2
  then [what_month(day2)]
  else what_month(day1)::month_range(day1 + 1, day2)
