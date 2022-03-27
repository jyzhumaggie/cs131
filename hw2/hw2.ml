type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal


(* Question 1 *)

(* extract rules with the a given lhs *)
let rec match_rules_w_same_nonterminals rules target_nonterminal = match rules with
    | [] -> []
    | hd::tl -> match hd with
        | (lhs_nt, rhs) -> 
                        if lhs_nt = target_nonterminal 
                        then rhs::(match_rules_w_same_nonterminals tl target_nonterminal)
                        else match_rules_w_same_nonterminals tl target_nonterminal

let convert_grammar gram1 = match gram1 with
    | (expression, rules) -> 
        let generating_function = match_rules_w_same_nonterminals rules in
        (expression, generating_function)


type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal


(* Question 2 *)
let parse_tree_leaves tree = 
    let rec parse_tree_structure tree_str = match tree_str with
        | [] -> []
        | hd::tl -> match hd with
            | Leaf leaf -> leaf::(parse_tree_structure tl)
            | Node (nt, subtree) -> (parse_tree_structure subtree) @ (parse_tree_structure tl)
    in parse_tree_structure [tree]


(* Question 3 *)

let match_done accept frag = accept frag

let rec matcher rule_generating_func rules accept frag = match rules with
    | [] -> None 
    | current_rule::rest_rules ->
        let match_current_rule = crosscheck_rule_element rule_generating_func current_rule accept frag in
            match match_current_rule with
                | None -> matcher rule_generating_func rest_rules accept frag
                | result -> result

and crosscheck_rule_element rule_generating_func rule accept frag = match rule with
    | [] -> match_done accept frag
    | _ -> (match frag with
        | [] -> None
        | hd::tl -> (match rule with 
            | [] -> None
            | (T t)::rest_rule_elements -> if t = hd then (crosscheck_rule_element rule_generating_func rest_rule_elements accept tl) else None
            | (N nt)::rest_rule_elements -> 
                    let next_rules = rule_generating_func nt in
                    let new_acceptor = crosscheck_rule_element rule_generating_func rest_rule_elements accept in
                    (matcher rule_generating_func next_rules new_acceptor frag)))

let make_matcher gram = match gram with
    | (start_symbol, rule_generating_func) -> 
        let rules = rule_generating_func start_symbol in
        fun accept frag -> matcher rule_generating_func rules accept frag


(* Question 4 *)


let rec matcher2 rule_generating_func rules accept frag = match rules with
    | [] -> None 
    | current_rule::rest_rules ->
        let match_current_rule = crosscheck_rule_element2 rule_generating_func current_rule accept frag in
            match match_current_rule with
                | None -> matcher2 rule_generating_func rest_rules accept frag
                | Some result -> Some (current_rule::result)

and crosscheck_rule_element2 rule_generating_func rule accept frag = match rule with
    | [] -> match_done accept frag
    | _ -> (match frag with
        | [] -> None
        | hd::tl -> (match rule with 
            | [] -> None
            | (T t)::rest_rule_elements -> if t = hd then (crosscheck_rule_element2 rule_generating_func rest_rule_elements accept tl) else None
            | (N nt)::rest_rule_elements -> 
                    let next_rules = rule_generating_func nt in
                    let new_acceptor = crosscheck_rule_element2 rule_generating_func rest_rule_elements accept in
                    (matcher2 rule_generating_func next_rules new_acceptor frag)))

(* def constructing_tree(rhs_traced, temp_root):
    if temp_root.is_node() and len(rhs_traced) > 0:
        # if this is a node, need to expand its child and remove one rule from the traced list
        tmp_rhs = rhs_traced[0]
        rhs_traced = rhs_traced[1:]
        temp_root.children = rhs2children(tmp_rhs)
        for i in range(len(temp_root.children)):
            (rhs_traced, temp_root.children[i]) = constructing_tree(rhs_traced, temp_root.children[i])
    return (rhs_traced, temp_root) *)
let rec constructing_tree rules = function 
(* start off using start_symbol as the root *)
    | [] -> (rules, [])
    (* search in depth, searching its children *)
    | hd::tl ->(let search_result = dfs rules hd in
            (* let search_result2 = dfs rules hd in *)
             match search_result with   
             (* horizontal search along one rule *)
            | (lhs, rhs) -> (let next_search_result = constructing_tree lhs tl in
                match next_search_result with
                | (lhs2, rhs2) -> (lhs2, rhs::rhs2)))
and dfs rules = function 
    | (N current_symbol) -> (match rules with 
        | [] -> ([], Node(current_symbol, []))
        | hd::tl -> (let search_result = constructing_tree tl hd in
            match search_result with
            | (lhs, rhs) -> (lhs, Node(current_symbol, rhs))))
    | (T current_symbol) -> (match rules with
        | [] -> ([], Leaf current_symbol)
        | hd::tl -> (hd::tl, Leaf current_symbol))


let make_parser gram = match gram with
    (* the rules we've used to expand the symbol list starting with the staring-symbol is:\n *)
    (* print("\nAnd obviously, the corresponding right-hand-side list is:\n") *)
    | (start_symbol, rule_generating_func) -> 
        fun frag -> let first_rules = rule_generating_func start_symbol in
                    let accept_empty_suffix suffix = if suffix = [] then Some [] else None in
                    let list_of_rules_used_in_order = matcher2 rule_generating_func first_rules accept_empty_suffix frag in 
                    match list_of_rules_used_in_order with
                    | Some [] -> None
                    | None -> None
    (* (_, tree) = constructing_tree(rhs_record, Node(start_symbol, [])) *)
                    | Some x -> let result = constructing_tree x [N start_symbol] in
                        match result with
                        | (_, tree) -> match tree with 
                            | [] -> None
                            | hd::tl -> Some hd 




















(* 

(* convert a list of rules with the same lhs into a list of just the rhs expansion *)
let rec constructing_list_of_same_lhs_rules same_lhs_list_of_rule_tuples = match same_lhs_list_of_rule_tuples with
    | [] -> []
    | hd::tl -> match hd with
        | (lhs_nt, rule) ->
                        rule::constructing_list_of_same_lhs_rules tl

let rec find_all_nts list_of_same_lhs_rules = match list_of_same_lhs_rules with
    | [] -> []
    | hd::tl -> match hd with 
        | [] -> []
        | current_t_or_nt::rest -> match current_t_or_nt with
            | N -> current_t_or_nt::


(* Finding a LIST of ALL TERMINALS given grammar style 1 *)
let rec find_all_terminals rules all_ts = match rules with
    | [] -> List.rev all_ts
    | hd::tl -> if not (List.mem (fst hd) all_ts) 
                then find_all_terminals tl ((fst hd)::all_ts)
                else find_all_terminals tl all_ts 
 *)

