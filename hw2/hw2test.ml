let accept_all string = Some string
let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x
   
   
type my_nonterminals =
  | Table | Basket | Fruits | Fruit

let my_table_grammar =
  (Table,
   function
     | Table ->
        [[T"|"; N Basket; N Fruits; T"|"];
        [T"|"; N Basket; T"|"];
        [T"|"; N Fruits; T"|"];
        [T"|"; T"|"]]
     | Basket ->
	    [[T"["; N Fruits; N Basket; N Fruits; T"]"];
       [T"["; N Basket; N Fruits; T"]"];
	     [T"["; N Fruits; N Basket; T"]"];
       [T"["; N Basket; T"]"];
	     [T"["; N Fruits; T"]"];
       [T"["; T"]"];]
     | Fruits ->
       [[N Fruit];
        (* [N Fruits]; *)
        [N Fruit; N Fruits]]
     | Fruit ->
       [[T"Apple"]; [T"Banana"]; [T"Orange"]; [T"Peach"]; [T"Pineapple"]])

(* Valid table *)
let my_table = ["|"; "["; "Banana"; "Orange"; "]"; "|"]
(* let make_matcher_test = ((make_matcher my_table_grammar accept_all my_table) = Some []) *)

(* Testing invalid Fruit string *)
(* let my_table = ["|"; "["; "Banana"; "Orange"; "Grape"; "]"; "|"] *)
(* let make_matcher_test = ((make_matcher my_table_grammar accept_all my_table) = None) *)


let my_table = ["|"; "["; "Banana"; "["; "Apple"; "["; "]"; "Pineapple"; "]"; "Orange"; "]"; "|"]
(* let make_matcher_test = ((make_matcher my_table_grammar accept_all my_table) = Some []) *)



let make_parser_test = 
  match (make_parser my_table_grammar my_table) with 
  | None -> false 
  | Some tree -> if parse_tree_leaves tree = my_table then true else false

