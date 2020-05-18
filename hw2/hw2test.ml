(* TAKEN FROM SAMPLE TEST CASE GIVEN *)
let accept_all string = Some string
let accept_empty_suffix = function 
	| _::_ -> None
	| x -> Some x


type my_nonterminals = 
	| Sent | Noun | Verb | Adj | Adv | Prep

let my_gram = 
	(Sent, 
		function 
			| Sent -> [[N Adj; N Noun; N Prep; N Verb];
				[N Adj; N Noun; N Verb];
				[N Noun; N Verb]]
			| Noun -> [[T"boy"];
				[T"girl"];
				[T"dog"];
				[T"cat"];
				[T"dinosaur"];
				[T"abacus"];
				[T"germany"];
				[T"hamstring"]]
			| Verb -> [[T"jumped"; N Adv];
				[T"ran"];
				[T"jumped"];
				[T"swam"];
				[T"cried"]]
			| Adv -> [[T"quickly"];[T"slowly"]]
			| Adj -> [[T"smart"];[T"dumb"];[T"slow"];[T"quick"]]
			| Prep -> [[T"with"; N Noun]])

	let make_matcher_test = ((make_matcher my_gram accept_empty_suffix ["smart";"dinosaur";"with";"cat";"jumped";"quickly"]) = Some [])

	let make_parser_test = match make_parser my_gram ["smart";"dinosaur";"with";"cat";"jumped";"quickly"] with
		| Some tree -> parse_tree_leaves tree = ["smart";"dinosaur";"with";"cat";"jumped";"quickly"]
		| _ -> false