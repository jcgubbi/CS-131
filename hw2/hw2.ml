type ('nonterminal, 'terminal) symbol = 
  | N of 'nonterminal
  | T of 'terminal;;
type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal;;

(*problem 1: convert hw1 type of grammar to hw2 type of grammar*)
let create_function rules value = 
	List.map (fun x -> (snd x)) (List.filter (fun x -> (fst x) = value) rules);;

let convert_grammar gram1 = match gram1 with 
	| (symbol, rules) -> (symbol, create_function rules);;

(*problem 2: find all leaves of a parse tree*)
let rec rev_parse_tree_leaves tree = match tree with
	| Leaf terminal -> [terminal]
	| Node (left, right) -> List.fold_left (fun acc x -> ((rev_parse_tree_leaves x) @ acc)) [] right;;

let parse_tree_leaves tree = List.rev (rev_parse_tree_leaves tree);;

(*problem 3: make matcher on grammar*)

(* function that checks to ensure that all is well *)
let rec valid gramF acceptor rule fragment = (match rule with 
	(* if we are out of rules then call acceptor*)
	| [] -> acceptor fragment
	(* else we have to still check rules, if the first is nonterminal then recurse using this as rule *)
	(* if the first is terminal then we can check if it matches our expression. if it does we can recurse *)
	(* while checking validity of the remaining parts. *)
	| (head::tail) -> (match head with 
		| (N nonterm) -> checkRules gramF (valid gramF acceptor tail) fragment nonterm (gramF nonterm)
		| (T term) -> (match fragment with 
			| [] -> None
			| (headF::tailF) -> (match (headF = term) with 
				| true -> valid gramF acceptor tail tailF 
				| false -> None))))
(* function that searches/calls other *)
and checkRules gramF acceptor fragment start ruleOptions = (match ruleOptions with
	(* if we are out of options then go ahead and return None *)
	(* else we check the validity using mutual recursion and if it returns a value then use it *)
	(* if it doesn't return a value then check with the next rule option *)
	| [] -> None
	| (head::tail) -> (match (valid gramF acceptor head fragment) with 
		| None -> checkRules gramF acceptor fragment start tail
		| Some x -> Some x));;

let make_matcher gram acceptor fragment = 
	checkRules (snd gram) acceptor fragment (fst gram) ((snd gram) (fst gram));;

(*problem 4: parse tree creator*)

(* simple acceptor that only allows perfect match *)
(* most code copied and pasted from make_matcher. main changes with passing trees *)

let acceptorNone suffix tree = match suffix with 
(* This allows us to return a TREE with our acceptor! *)
	| _::_ -> None
	| x -> Some tree;;
let rec appendSubtreeRecurse gramF  acceptor tail start currTree frag tree = 
(* curried function to allow me to append a tree to another tree *)
	validParse gramF acceptor tail start frag (currTree@[tree])
and validParse gramF acceptor rule start fragment tree = (match rule with 
	| [] -> acceptor fragment (Node(start, tree))
	| (head::tail) -> (match head with 
		(* There I used the curreid acceptor and also passed an empty tree! this allows me to only add to the tree if accepted *)
		(* and for not all changes to be added to my final tree *)
		(* at any point i can basically disregard whatever tree i have built. *)
		| (N nonterm) -> checkRulesParse gramF (appendSubtreeRecurse gramF acceptor tail start tree) fragment nonterm (gramF nonterm) []
		| (T term) -> (match fragment with 
			| [] -> None
			| (headF::tailF) -> (match (headF = term) with 
				(* simply add the leaf node with the terminal value onto the tree *)
				| true -> validParse gramF acceptor tail start tailF (tree@[Leaf term])
				| false -> None))))
(* function that searches/calls other *)
and checkRulesParse gramF acceptor fragment start ruleOptions tree = (match ruleOptions with
	| [] -> None
	(* same as make_matcher (maybe I could have used this code in common but seemed simpler to copy bc of mutual recursion) *)
	| (head::tail) -> (match (validParse gramF acceptor head start fragment tree) with 
		| None -> checkRulesParse gramF acceptor fragment start tail tree
		| Some x -> Some x));;
let make_parser gram fragment = 
	checkRulesParse (snd gram) acceptorNone fragment (fst gram) ((snd gram) (fst gram)) [];;
