(* FALL 2021 CS131 HW1 
    Question 1 through 6, and testcases for question 8 were done by me individually;
    I used https://github.com/melodychn/CS-131/blob/master/HW1/hw1.ml as a reference for Question 7.
*)


let rec equal_length l1 l2 = 
	match (l1, l2) with
	| ([], []) -> true
	| (_::t1, _::t2) -> equal_length t1 t2
	| _ -> false


(* 1. subset a b: return true iff a⊆b *)
let rec subset a b =    
  	match a with 
  	| [] -> true
  	| h::t -> if List.exists (fun x-> x = h) b 
	  	then subset t b 
		else false
  
(* 2. equal_sets a b: tests if two sets are equal *)
let equal_sets a b = 
  if subset a b && subset b a 
  then true 
  else false

(* 3. find the union of two sets *)
let rec set_union a b = 
	match a with
  	| [] -> b 
  	| h::t -> h::(set_union t b)
  

(* 4. find the union of all a set of sets *)
let rec set_all_union a = 
  	match a with
  	| [] -> []
  	| h::t -> h@(set_all_union t)

(* 5. Russell's Paradox *)
(* Self_member s cannot be written in OCaml. 
A set which contains itself does not have the same type as itself.  
Different types of data cannot be compared in OCaml. *)

(* 6. computed_fixed_point eq f x finds the fixed point of a function *)
let rec computed_fixed_point eq f x = 
    if (eq (f x) x) 
	then x
    else computed_fixed_point eq f (f x)  


(* 7. filter_reacheable *)

type ('nonterminal, 'terminal) symbol = 
	| N of 'nonterminal 
	| T of 'terminal


(* list of symbols -> list of non-terminal symbols *)
let rec filter_symbols list = 
match list with
| [] -> []
| first::rest -> 
    match first with
    | N x -> x::(filter_symbols rest)
    | _ -> filter_symbols rest




let rec filter_rules_list rules = 
	match rules with 
	| [] -> []
	| first::rest -> 
    	match first with
    	| (sym, expansion) -> (set_union (filter_symbols expansion) (filter_rules_list rest))



(* list of nonterminal symbols-> reachable nonterminal symbols *)
let rec find_nonterminal_all lst des = 
  	match lst with
  	| [] -> []
  	| x::rest -> let matching_symbols = 
	  				x::(filter_rules_list (List.filter (fun y -> match y with 
	  				| (sym, rhs) -> if sym = x then true else false) des)) in 
	  (set_union matching_symbols (find_nonterminal_all rest des))





(* list of reachable nonterminals *)
let rec get_all_nonterminals symb filtered_grammar rules = 
	let list = List.filter(fun x -> not (List.mem x symb)) filtered_grammar in
  	match list with 
  	| [] -> filtered_grammar
  	| _ -> get_all_nonterminals filtered_grammar(set_union (find_nonterminal_all list rules) filtered_grammar) rules

(* compares list to g to remove unreachable *)
let rec comp_create_final_list grammar all_nt =
  match grammar with
  | (symb, rules) -> match rules with
    | [] -> []
    | hd::tl -> match hd with
      | (nt, rhs) -> if (subset [nt] all_nt) 
        then (nt, rhs)::(comp_create_final_list (symb, tl) all_nt)
        else comp_create_final_list (symb, tl) all_nt

let filter_reachable g = 
	match g with
	| (a, b) -> let matching_symbols = a::(filter_rules_list 
				(List.filter 
				(fun x -> match x with 
	  					| (sym, rhs) -> if sym = a then true else false) b)) in 
	let all = get_all_nonterminals [a] matching_symbols b in
  	(a, compare_list g all)