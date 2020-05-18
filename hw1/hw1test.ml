let my_subset_test0 = subset [0;2;3] [2;3;4;5;6;0];;
let my_subset_test1 = not (subset [0;1;3;5] [1;3;5;1;2;5]);;

let my_equal_sets_test0 = equal_sets [0] [0; 0; 0;];;
let my_equal_sets_test1 = not (equal_sets [0; 1; 2;] [0;2]);;

let my_set_union_test0 = equal_sets (set_union [0; 2; 4; 1] [1; 3; 3]) [0; 1; 2; 3; 4];;
let my_set_union_test1 = not (equal_sets (set_union [0; 1; 2] [2; 3; 4]) [0; 1; 3; 4]);;

let my_set_intersection_test0 = equal_sets (set_intersection [0; 1; 2; 3] [2; 3; 4; 5]) [2; 3];;
let my_set_intersection_test1 = not (equal_sets (set_intersection [1; 5; 2] [3; 4; 5]) [2; 5]);;

let my_set_diff_test0 = equal_sets (set_diff [0; 1; 2; 3] [0; 3]) [1; 2];;
let my_set_diff_test1 = not (equal_sets (set_diff [0; 1; 2; 4] [4; 6; 7; 2]) [0;1;2;4]);;

let my_computed_fixed_point_test0 = 
	computed_fixed_point (=) (fun x-> x *. 3.) 1.2 = infinity;;
let my_computed_fixed_point_test1 = 
	computed_fixed_point (=) (fun x-> x/10) 234 = 0;;

type nonterminals = | A | B | C | D;;

let rules = 
	[A, [T"<"; N B; T">"];
	B, [T"<"; N C; T"H"];
	C, [N B; N A];
	C, [T"r"];
	D, [T"r"; T"E"]];;

let grammar = A, rules;;

let my_filter_reachable_test0 = 
	filter_reachable grammar = 
	(A, 
		[A, [T"<"; N B; T">"];
		B, [T"<"; N C; T"H"];
		C, [N B; N A];
		C, [T"r"];]);;