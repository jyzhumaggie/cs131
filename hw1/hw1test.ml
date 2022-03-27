
(* Q1 *)
let my_subset_test_0 = subset [] []
let my_subset_test_1 = subset [] [1;2]
let my_subset_test_2 = subset [1] [1;2;1]
let my_subset_test_3 = subset [1] [2]
let my_subset_test_4 = subset [1;2] [2;3]
let my_subset_test_5 = subset [1] []


(* Q2 *)
let my_equal_sets_test_0 = equal_sets [] []
let my_equal_sets_test_1 = equal_sets [1] [1]
let my_equal_sets_test_2 = equal_sets [1;1] [1]
let my_equal_sets_test_3 = equal_sets [1;2;2] [1;2]
let my_equal_sets_test_4 = equal_sets [2;3] [3;2]
let my_equal_sets_test_5 = equal_sets [1] [3]


(* Q3 *)
let my_set_union_test_0 = set_union [] []
let my_set_union_test_1 = set_union [1;2] [2]
let my_set_union_test_2 = set_union [1] []
let my_set_union_test_3 = set_union (set_union [19;3] [2]) [1]
let my_set_union_test_4 = set_union (set_union [] []) (set_union [] [1])
let my_set_union_test_5 = set_union [1;2] (set_union (set_union [1] [2]) [3])


(* Q4 *)
let my_set_all_union_test0 = equal_sets (set_all_union []) []
let my_set_all_union_test1 = equal_sets (set_all_union [[1;2]]) [2;1]
let my_set_all_union_test2 = equal_sets (set_all_union [[3;1;3]; [4]; [1;2;3]]) [1;2;3;4]
let my_set_all_union_test3 = equal_sets (set_all_union [[4;2]; []; [5;2]; [3;5;7]]) [2;3;4;5;7]
let my_set_all_union_test4 = equal_sets (set_all_union [[]; []]) []
let my_set_all_union_test5 = equal_sets (set_all_union [[3];[2];[5];[7]]) [2;3;5;7]

(* Q6 *)
let my_computed_fixed_point = computed_fixed_point (=) (fun x -> x /. 4.) 16. = 0.
let my_computed_fixed_point = computed_fixed_point (=) (fun x -> x *. 4.) 16. = infinity
(* let computed_fixed_point_test = computed_fixed_point (=) (fun x -> (x + 1) mod 5) 3 = 1;; will run infinitely *)


(* 7 *)
type giant_nonterminals =
  | Conversation | Sentence | Grunt | Snore | Shout | Quiet

let giant_grammar =
  Conversation,
  [Snore, [T"ZZZ"];
   Quiet, [];
   Grunt, [T"khrgh"];
   Shout, [T"aooogah!"];
   Sentence, [N Quiet];
   Sentence, [N Grunt];
   Sentence, [N Shout];
   Conversation, [N Snore];
   Conversation, [N Sentence; T","; N Conversation]]

let giant_test0 =
  filter_reachable giant_grammar = giant_grammar
let giant_test1 = 
  filter_reachable (Sentence, snd giant_grammar) = (Sentence,
   [(Quiet, []); (Grunt, [T "khrgh"]); (Shout, [T "aooogah!"]);
    (Sentence, [N Quiet]); (Sentence, [N Grunt]); (Sentence, [N Shout])])
let giant_test2 =
  filter_reachable (Quiet, snd giant_grammar) = (Quiet, [Quiet, []])
let giant_test3 =
  filter_reachable (Snore, snd giant_grammar) = (Snore, [(Snore, [T "ZZZ"])]);;


