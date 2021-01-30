type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

(*Problem 1*)
let rec extract_list rules_list target  = match rules_list with
	| [] -> []
	| start::rest -> if (fst start) = target then (snd start) ::(extract_list rest target)
        else extract_list rest target;;

let convert_grammar graml =
    ((fst graml), fun nt -> extract_list (snd graml) nt);;

(*Problem 2*)
let rec set_union a b = match a with
    |  [] -> b
    |  start::rest -> set_union rest (start::b);;

let rec find_leaves tree_list = match tree_list with
	| [] -> []
	| start::rest -> match start with
		| Leaf leaf -> leaf::find_leaves rest
		| Node (nonterm, subtree) -> find_leaves subtree @ (find_leaves rest);;

let rec parse_tree_leaves tree = find_leaves [tree];;

(*Problem 3*)
(*let accept_all string = Some string;;*)
(*let accept_empty_suffix x  = match x with*)
(*   | _::_ -> None*)
(*   | x -> Some x;;*)

let rec make_match_helper gram_func rules_list acceptor fragment = match rules_list with
	| [] -> None
	| start::rest ->
		let result = match_rule gram_func start acceptor fragment in
		if result = None then make_match_helper gram_func rest acceptor fragment
		else result
and match_rule gram_func rule acceptor fragment = match rule with
	| [] -> acceptor fragment
	| rule_start::rule_rest -> match rule_start with
		| N sym ->
			let cur_rules = gram_func sym in
			let acceptor2 = match_rule gram_func rule_rest acceptor in
			make_match_helper gram_func cur_rules acceptor2 fragment
		| T sym -> match fragment with
			|[] -> None
			|fragment_start::fragment_rest -> 
                if fragment_start = sym 
                    then match_rule gram_func rule_rest acceptor fragment_rest					
                else None;;


let make_matcher graml = make_match_helper (snd graml) ((snd graml) (fst graml));;

(*Problem 4*)
let parse_acceptor path fragment = match fragment with
	|[] -> Some path
	|_ -> None;;

(*Iterate through the list of rules*)
let rec parse_rules_list gram_func rules_list start_sym acceptor path fragment = match rules_list with
	|[] -> None
	|start::rest ->
		(* Result of matching this particular rule *)
		let result = parse_rule gram_func start acceptor ((start_sym, start)::path) fragment in
		(match result with
			(* None means that there was no match found for the rule, so we can move onto the next rule *)
			|None -> parse_rules_list gram_func rest start_sym acceptor path fragment
			(* Anything else means that there was a suffix remaining even though we matched the rule *)
			|_ -> result
		)
(* Iterate through each symbol of the particular rule *)
and parse_rule gram_func rule acceptor path fragment = match rule with
	|[] -> acceptor path fragment
	|rule_start::rule_rest -> match rule_start with
		(* Found a nonterminal symbol, so recurse and go one layer deeper *)
		|N sym ->
			let cur_rules = gram_func sym in
			let acceptor2 = parse_rule gram_func rule_rest acceptor in
			(* Recursively call the function with new set of rules and start symbol *)
			parse_rules_list gram_func cur_rules sym acceptor2 path fragment
		(* Found a terminal symbol, so check to see if it matches our fragment *)
		|T sym -> match fragment with
			|[] -> None
			(* If they match, recursively call this function to check the next symbols in the rule and fragment *)
			|fragment_start::fragment_rest -> if fragment_start = sym then parse_rule gram_func rule_rest acceptor path fragment_rest
									else None;;

(* Make the parse tree, given a path *)
let make_tree path_maker fragment =
	let path = path_maker fragment in
	match path with
	|Some x ->
		let in_path = List.rev x in
		(* Make a tree using the start of the path *)
		let rec make_tree_helper path = match path with
		|path_hd::path_tl ->
			let symbol = fst path_hd in
			let rule = snd path_hd in
			(* Get siblings of the current symbol *)
			let ret = get_children path_tl rule in
			let path_remaining = fst ret in
			let children = snd ret in
			path_remaining, Node (symbol, children)
		(* Gets the same level in the tree, which are all siblings *)
		and get_children path_remaining rule = match rule with
		|[] -> path_remaining, []
		|rule_start::rule_rest ->
			(match rule_start with
			(* If it's a nonterminal symbol, we need to construct the tree at the symbol and append it to sibling trees *)
			|N sym ->
				let make_tree_result = make_tree_helper path_remaining in
				let next_path_remaining = fst make_tree_result in
				let curr_tree = snd make_tree_result in
				let get_children_result = get_children next_path_remaining rule_rest in
				let next_next_path_remaining = fst get_children_result in
				let siblings = snd get_children_result in
				next_next_path_remaining, curr_tree::siblings
			(* If it's a terminal symbol, we just need to append it to its siblings *)
			|T sym ->
				let result = get_children path_remaining rule_rest in
				let next_path_remaining = fst result in
				let siblings = snd result in
				next_path_remaining, (Leaf sym)::siblings) in
		Some (snd (make_tree_helper in_path))
	|_ -> None;;

let make_parser gram =
	let path_maker = parse_rules_list (snd gram) ((snd gram) (fst gram)) (fst gram) parse_acceptor [] in
	make_tree path_maker;;