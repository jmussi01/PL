
(* 1a *)
val abc = ["a", "b", "c"];
val ab = ["a", "b"];
all_except_option("c", []) = NONE;
all_except_option("f", abc) = NONE;
all_except_option("b", ab) = SOME ["a"];
all_except_option("a", ab) = SOME ["b"];
all_except_option("b", abc) = SOME ["a", "c"];
(* 1b *)
get_substitutions1([], "Fred") = [];
get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], 
"Fred") = ["Fredrick","Freddie","F"];
get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], 
"Jeff") = ["Jeffrey","Geoff","Jeffrey"];
(* 1c *)
get_substitutions2([], "Fred") = [];
get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], 
"Fred") = ["Fredrick","Freddie","F"];
get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], 
"Jeff") = ["Jeffrey","Geoff","Jeffrey"]; 
(* 1d *)
similar_names([], {first="Fred", middle="W", last="Smith"}) = [{first="Fred", last="Smith", middle="W"}];
similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
{first="Fred", middle="W", last="Smith"}) = [{first="Fred", last="Smith", middle="W"},
					     {first="Fredrick", last="Smith", middle="W"},
					     {first="Freddie", last="Smith", middle="W"},
					     {first="F", last="Smith", middle="W"}];
(* 2a *)
card_color(Clubs, Jack) = Black;
card_color(Diamonds, Jack) = Red;
card_color(Hearts, Jack) = Red;
card_color(Spades, Jack) = Black;

(* 2b *)
card_value(Clubs, Jack) = 10;
card_value(Clubs, Queen) = 10;
card_value(Clubs, King) = 10;
card_value(Clubs, Ace) = 11;
card_value(Clubs, Num 7) = 7;

(* 2c *)
val two_cards = [(Clubs, Jack), (Clubs, Queen)];
val three_cards = [(Clubs, Jack), (Clubs, Queen), (Clubs, King)];
val four_cards = [(Clubs, Jack), (Clubs, Queen), (Clubs, Queen), (Clubs, King)];
val four_more_cards = [(Clubs, Jack), (Spades, Queen), (Diamonds, Queen), (Clubs, King)];
(* remove_card([], (Clubs, Ace), NoMatch); *)
(* remove_card(two_cards, (Clubs, Ace), IllegalMove); *)
remove_card(two_cards, (Clubs, Queen), NoMatch) = [(Clubs, Jack)];
remove_card(two_cards, (Clubs, Jack), NoMatch) = [(Clubs, Queen)];
remove_card(three_cards, (Clubs, Queen), IllegalMove) = [(Clubs, Jack), (Clubs, King)];
remove_card(four_cards, (Clubs, Queen), NoMatch) = [(Clubs, Jack), (Clubs, Queen), (Clubs, King)];

(* 2d *)
(* all_same_color([]); *)
all_same_color(two_cards) = true;
all_same_color([(Clubs, Jack), (Diamonds, Queen)]) = false;
all_same_color(four_cards) = true;
all_same_color(four_more_cards) = false;

(* 2e *)
sum_cards([]) = 0;
sum_cards(four_cards) = 40;
sum_cards([(Clubs, King), (Clubs, Num 7)]) = 17;

(* 2f *)
score(four_cards, 0) = 60;
score(four_cards, 50) = 5;
score(four_more_cards, 0) = 120;
score(four_more_cards, 50) = 10;

(* 2g *) (*
officiate(four_cards, [], 10) = score(four_cards, 10);
officiate(four_cards, [Discard(Clubs, Queen)], 17) = score(three_cards, 17);
fun provided_test1 () = (* correct behavior: raise IllegalMove *)
    let val cards = [(Clubs,Jack),(Spades,Num(8))]
	val moves = [Draw,Discard(Hearts,Jack)]
    in
	officiate(cards,moves,42)
    end
fun provided_test2 () = (* correct behavior: return 3 *)
    let val cards = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)]
	val moves = [Draw,Draw,Draw,Draw,Draw]
    in
 	officiate(cards,moves,42)
    end (*
provided_test1(); 
provided_test2() = 3; *) *)
