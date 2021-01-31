type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

(*Problem 1*)
let rec extract_list altern_list target  = match altern_list with
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
let rec make_match_helper altern_list production_func acceptor fragment = match altern_list with
	| [] -> None
	| start::rest ->
		let remain_and_children = rule_matcher start production_func acceptor fragment in
		if remain_and_children != None
		    then remain_and_children
        else make_match_helper rest production_func acceptor fragment
    and rule_matcher rule production_func acceptor fragment = match rule with
	| [] -> acceptor fragment
	| rule_start::rule_rest -> match rule_start with
    (*	Check the symbols in the rule one by one to pass the acceptor*)
		| N symb ->
			let cur_rules = production_func symb in
			let acceptor2 = rule_matcher rule_rest production_func acceptor in
			make_match_helper cur_rules production_func acceptor2 fragment
		| T symb ->
		    match fragment with
			| [] -> None
			| fragment_start::fragment_rest ->
                if fragment_start = symb 
                    then rule_matcher rule_rest production_func acceptor fragment_rest
                else None;;

let make_matcher graml = make_match_helper ((snd graml) (fst graml)) (snd graml);;

(*Problem 4*)
let rec tree_maker path = match path with
    | path_start::path_rest ->
        let symbol = fst path_start in
        let rule = snd path_start in
        let remain_and_children = find_children rule path_rest in
        let children = snd remain_and_children in
        let remained_paths = fst remain_and_children in
        remained_paths, Node (symbol, children)
and find_children rule remained_paths = match rule with
    | [] -> remained_paths, []
    | rule_start::rule_rest ->
        (match rule_start with
        | N symb ->
            let tree_maker_res = tree_maker remained_paths in
            let next_remained_paths = fst tree_maker_res in
            let cur_tree = snd tree_maker_res in
            let remain_and_children = find_children rule_rest next_remained_paths in
            let next_next_path_remaining = fst remain_and_children in
            let siblings = snd remain_and_children in
            next_next_path_remaining, cur_tree::siblings
        | T symb ->
            let remain_and_children = find_children rule_rest remained_paths in
            let next_remained_paths = fst remain_and_children in
            let siblings = snd remain_and_children in
            next_remained_paths, (Leaf symb)::siblings);;

let tree_maker_entry path_info fragment =
	let path = path_info fragment in
	match path with
	| Some cur_path ->
		let rev_path = List.rev cur_path in
		Some (snd (tree_maker rev_path))
	| _ -> None;;

let rec make_parser_helper start_symbol altern_list production_func acceptor path fragment = match altern_list with
	| [] -> None
	| start::rest ->
		let result = rule_parser start production_func acceptor ((start_symbol, start)::path) fragment in
		(match result with
			| None -> make_parser_helper start_symbol rest production_func acceptor path fragment
			| _ -> result
		)
    and rule_parser rule production_func acceptor path fragment = match rule with
	| [] -> acceptor path fragment
	| rule_start::rule_rest -> match rule_start with
		| N symb ->
			let cur_rules = production_func symb in
			let acceptor2 = rule_parser rule_rest production_func acceptor in
			make_parser_helper symb cur_rules production_func acceptor2 path fragment
		| T symb -> match fragment with
			| [] -> None
			| fragment_start::fragment_rest ->
			    if fragment_start = symb
			        then rule_parser rule_rest production_func acceptor path fragment_rest
				else None;;

let parse_acceptor path fragment = match fragment with
	| [] -> Some path
	| _ -> None;;

let make_parser gram =
	let path_info = make_parser_helper (fst gram) ((snd gram) (fst gram)) (snd gram) parse_acceptor [] in
	tree_maker_entry path_info;;