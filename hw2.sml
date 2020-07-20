fun same_string(s1 : string, s2 : string) =
    s1 = s2
		      
fun all_except_option (str, lst) =
    case lst of
      [] => NONE
    | str'::lst' => if same_string(str,str')
		    then SOME lst'
		    else case all_except_option (str, lst') of
			     NONE => NONE
			   | SOME lst'' => SOME (str'::lst'')


fun get_substitutions1 (substitutions, str)=
    case substitutions of
	[] => []
      | list::list_lists => case all_except_option(str,list) of
				    NONE => get_substitutions1(list_lists, str)
				  | SOME v => v @ get_substitutions1(list_lists, str)


fun get_substitutions2 (substitutions, str) =
  let fun helper(substitutions, acc) =
	  case substitutions of
	      [] => acc
	    | list::list_lists => case all_except_option(str,list) of	
				      NONE => helper(list_lists, acc)
				    | SOME v => helper(list_lists, v@acc)
  in
      helper(substitutions, [])
  end

      
fun similar_names (subs, name) = 
  let val {first=xn, middle=yn, last=zn} = name
      fun helper (lst, acc) =
        case lst of
        [] => name::acc
        | x::xs => helper(xs, {first=x, middle=yn, last=zn}::acc)
  in helper(get_substitutions2(subs, xn), [])
  end    

      
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

fun card_color (card') =
    case card' of
	(Clubs,_) => Black
      | (Spades,_) => Black
      | (Diamonds,_) => Red
      | (Hearts,_) => Red


fun card_value (card') =
    case card' of
	(_, Jack) => 10
      | (_,Queen) => 10
      | (_,King) => 10
      | (_,Ace) => 11
      | (_,Num k) => k
			  
			
fun remove_card (cs, c, e) =
  case cs of
      c' :: cs_minus_c => if c' = c
		  then cs_minus_c
		  else c' :: remove_card(cs_minus_c, c, e)
    | [] => raise e

		  
fun all_same_color (cs) =
    case cs of
	[] => true
      | c::cs_minus_c => case cs_minus_c of
			     [] => true 
			   | c'::cs_minus_c' => if card_color(c) = card_color(c')
						then all_same_color(cs_minus_c)
						else false
				       			

fun sum_cards (cs) =
    case cs of
	c::cs_minus_c => card_value(c) + sum_cards(cs_minus_c)
     | [] => 0

		 
fun score (held_cards, goal) =
    let
	val sum = sum_cards(held_cards)
	fun pre_score (held_cards, goal) =
	  if sum > goal
	  then 3*(sum - goal)
	  else if sum = 0
	  then 0
	  else (goal - sum)
    in
	if all_same_color(held_cards) = false
	then pre_score(held_cards, goal)
	else pre_score(held_cards, goal) div 2			 			      
    end

(*

fun officiate (card_list, move_list, goal) =
    let
	fun game_state (card_list, move_list, held_list, goal) =  
	    case move_list of
		[] => score (held_list, goal)
	      | discard::rest_of_moves => case discard of
					      Discard c
					      
					    
			     
		
    in

 *)
	
			 
				
			    

	   
	
	
					    
				       
		  
