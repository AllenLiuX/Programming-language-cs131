type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

(* Problem 1 *)
let rec find_in_list rules_list type_param  = match rules_list with
	|[] -> []
	|hd::tl -> if (fst hd) = type_param then (snd hd) :: find_in_list tl type_param
				else find_in_list tl type_param;;

let convert_grammar graml = ((fst graml), find_in_list (snd graml));;

(* Problem 2 *)
let rec parse_tree_helper tree_list = match tree_list with
	|[] -> []
	|hd::tl -> match hd with
		|Leaf leaf -> leaf::parse_tree_helper tl
		|Node (nonterm, subtree) -> parse_tree_helper subtree @ parse_tree_helper tl;;

let rec parse_tree_leaves tree = parse_tree_helper [tree];;

(* Problem 3 *)
let accept_all string = Some string;;
let accept_empty_suffix x  = match x with
   | _::_ -> None
   | x -> Some x;;

let rec match_rules_list gram_func rules_list acceptor fragment = match rules_list with
	|[] -> None
	|hd::tl ->
		let result = match_rule gram_func hd acceptor fragment in
		if result = None then match_rules_list gram_func tl acceptor fragment
		else result
and match_rule gram_func rule acceptor fragment = match rule with
	|[] -> acceptor fragment
	|rule_hd::rule_tl -> match rule_hd with
		|N sym ->
			let next_rules = gram_func sym in
			let new_acceptor = match_rule gram_func rule_tl acceptor in
			match_rules_list gram_func next_rules new_acceptor fragment
		|T sym -> match fragment with
			|[] -> None
			|frag_hd::frag_tl -> if frag_hd = sym then match_rule gram_func rule_tl acceptor frag_tl
									else None;;

let make_matcher graml = match_rules_list (snd graml) ((snd graml) (fst graml));;

(* Problem 4 *)
let parse_acceptor path fragment = match fragment with
	|[] -> Some path
	|_ -> None;;

(* Iterate through the list of rules*)
let rec parse_rules_list gram_func rules_list start_sym acceptor path fragment = match rules_list with
	|[] -> None
	|hd::tl ->
		(* Result of matching this particular rule *)
		let result = parse_rule gram_func hd acceptor ((start_sym, hd)::path) fragment in
		(match result with
			(* None means that there was no match found for the rule, so we can move onto the next rule *)
			|None -> parse_rules_list gram_func tl start_sym acceptor path fragment
			(* Anything else means that there was a suffix remaining even though we matched the rule *)
			|_ -> result
		)
(* Iterate through each symbol of the particular rule *)
and parse_rule gram_func rule acceptor path fragment = match rule with
	|[] -> acceptor path fragment
	|rule_hd::rule_tl -> match rule_hd with
		(* Found a nonterminal symbol, so recurse and go one layer deeper *)
		|N sym ->
			let next_rules = gram_func sym in
			let new_acceptor = parse_rule gram_func rule_tl acceptor in
			(* Recursively call the function with new set of rules and start symbol *)
			parse_rules_list gram_func next_rules sym new_acceptor path fragment
		(* Found a terminal symbol, so check to see if it matches our fragment *)
		|T sym -> match fragment with
			|[] -> None
			(* If they match, recursively call this function to check the next symbols in the rule and fragment *)
			|frag_hd::frag_tl -> if frag_hd = sym then parse_rule gram_func rule_tl acceptor path frag_tl
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
		|rule_hd::rule_tl ->
			(match rule_hd with
			(* If it's a nonterminal symbol, we need to construct the tree at the symbol and append it to sibling trees *)
			|N sym ->
				let make_tree_result = make_tree_helper path_remaining in
				let next_path_remaining = fst make_tree_result in
				let curr_tree = snd make_tree_result in
				let get_children_result = get_children next_path_remaining rule_tl in
				let next_next_path_remaining = fst get_children_result in
				let siblings = snd get_children_result in
				next_next_path_remaining, curr_tree::siblings
			(* If it's a terminal symbol, we just need to append it to its siblings *)
			|T sym ->
				let result = get_children path_remaining rule_tl in
				let next_path_remaining = fst result in
				let siblings = snd result in
				next_path_remaining, (Leaf sym)::siblings) in
		Some (snd (make_tree_helper in_path))
	|_ -> None;;

let make_parser gram =
	let path_maker = parse_rules_list (snd gram) ((snd gram) (fst gram)) (fst gram) parse_acceptor [] in
	make_tree path_maker;;