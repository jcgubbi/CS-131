(*PROBLEM 1: SUBSET*)
let rec subset a b = match a with 
	| [] -> true
	| head::tail -> 
		if List.exists (fun x -> x = List.hd a) b then subset (List.tl a) b
		else false;;

(*PROBLEM 2: EQUAL SETS*)
let equal_sets a b = 
	(subset a b) && (subset b a);;

(*PROBLEM 3: SET UNION*)
let rec set_union a b = match a with
	| [] -> b
	| head::tail -> 
		if List.exists(fun x -> x = List.hd a) b then set_union (List.tl a) b
		else set_union (List.tl a) ((List.hd a)::b);;

(*PROBLEM 4: SET INTERSECTION*)
let set_intersection a b = 
	List.filter(fun x -> List.exists(fun y-> y = x) b) a;;

(*PROBLEM 5: SET DIFFERENCE*)
let set_diff a b = 
	List.filter(fun x -> not(List.exists(fun y-> y = x) b)) a;;

(*PROBLEM 6: COMPUTED FIXED POINT*)
let rec computed_fixed_point eq f x = 
	if eq (f x) x then x
	else computed_fixed_point eq f (f x);;

(*PROBLEM 7: FILTER REACHABLE RULES*)
type ('nonterminal, 'terminal) symbol = 
	| N of 'nonterminal
	| T of 'terminal;;

let ruleIsTerminal rule = match rule with
	| N nonterminal -> true
	| T terminal -> false;;

let filter_nonterminal listRules = 
	List.filter ruleIsTerminal listRules;;

let rec only_reachable goodSymbols listRules = match listRules with 
	| [] -> goodSymbols
	| head::tail -> 
		let firstSymbol = (fst head) in 
		let restExpression = (snd head) in
		if List.mem (N firstSymbol) goodSymbols then 
			let nonterm = filter_nonterminal restExpression in
			let union = set_union nonterm goodSymbols in 
			only_reachable union tail
		else
			only_reachable goodSymbols tail;;

let rec recursive_find goodSymbols listRules = 
	let x = only_reachable goodSymbols listRules in 
	if equal_sets x goodSymbols then x
	else recursive_find x listRules;;

let filter_reachable g = match g with 
	| (firstSymbol, listRules) -> 
		let symbols = recursive_find [N firstSymbol] listRules in
		let filteredRules = List.filter(fun valid -> let valid = (N (fst valid)) in List.mem valid symbols) listRules in
		(firstSymbol, filteredRules);;

