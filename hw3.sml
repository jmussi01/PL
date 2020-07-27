fun only_capitals (ls) =
    List.filter (fn string => Char.isUpper(String.sub(string,0))) ls

		
fun longest_string1 (ls) =
    List.foldl (fn (x,y) => if String.size(x) > String.size(y)
			    then x
			    else y)
	       "" ls
					 

fun longest_string2 (ls) =
    List.foldl (fn (x,y) => if String.size(x) >= String.size(y)
			    then x
			    else y)
	       "" ls

fun longest_string_helper f ls = 
    List.foldl (fn (x,y) => if f(String.size x, String.size y)
			    then x
			    else y)
	       "" ls

val longest_string3 = longest_string_helper (fn (x,y) => x > y)

val longest_string4 = longest_string_helper (fn (x,y) => x >= y)

(* val longest_capitalized  = longest_string2 o only_capitals *)
fun longest_capitalized(ls) =
    let
        val l_c = longest_string1 o only_capitals
    in
        l_c(ls)
    end
	
fun rev_string string =
    (String.implode o List.rev o String.explode) string
    
exception NoAnswer	
fun first_answer f ls =
    case ls of
	[] => raise  NoAnswer
      | s::ls' => case f s of
		      NONE => first_answer f ls
		    | SOME v => v
			      
fun all_answers f ls =
    let
        fun f_n (f, ls, acc) = case ls of
                                      [] => SOME acc
                                    | x::xs => case f(x) of
                                                   NONE => NONE
                                                 | SOME v => f_n(f, xs, v @ acc)
    in
        f_n(f, ls, [])
    end



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


datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string


fun count_wildcards (p) =
    g (fn v => 1) (fn v => 0) p
			     
fun count_wild_and_variable_lengths (p) =
    g (fn v => 1) (fn s => String.size s) p

fun count_some_var (string, p) =
    g (fn v => 0) (fn s => if s = string then 1 else 0) p


 fun check_pat (p: pattern) =
    let
        fun variables(p: pattern) =
	    case p of
	        Variable x        => [x]
	      | TupleP ps         => List.foldl (fn (v,vs) => vs @ variables(v)) [] ps
	      | ConstructorP(_,p) => variables(p)
	      | _                 => []
        fun repeats(sl: string list) =
            case sl of
                [] => true
              | x::xs => if List.exists (fn a => a = x) xs then false else repeats(xs)
    in
        repeats(variables(p))
    end


fun match (v, p) =
    case p of
        Variable x => SOME [(x, v)]
      | UnitP =>
        (case v of
             Unit => SOME []
           | _ => NONE)
      | Wildcard => SOME []
      | ConstP k =>
        (case v of
             Const(v) => if k = v then SOME [] else NONE
           | _ => NONE)
      | TupleP ps =>
        (case v of
             Tuple(vs) => if List.length vs = List.length ps
                          then all_answers match (ListPair.zip(vs, ps))
                          else NONE
           | _ => NONE)
      | ConstructorP(s1,pp) =>
        (case v of
             Constructor(s2,vv) =>
             if s1 = s2 then match(vv,pp) else NONE
           | _ => NONE)
            

fun first_match v ps =
    SOME(first_answer (fn p => match(v, p)) ps) handle NoAnswer => NONE
    
