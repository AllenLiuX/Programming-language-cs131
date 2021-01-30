(*problem 1*)
let rec contain a b = match b with
    |  [] -> false
    |  start::rest -> (a=start) || (contain a rest);;

let rec subset a b = match a with
    |  [] -> true
    |  start::rest -> (contain start b) && (subset rest b);;

(*problem 2*)
let equal_sets a b = (subset a b) && (subset b a);;

(*problem 3*)
let rec set_union a b = match a with
    |  [] -> b
    |  start::rest -> set_union rest (start::b);;

(*problem 4*)
(*return elements in a that are not in b*)
let rec set_diff a b = match a with
    | []->[]
    | start::rest->
    if not(contain start b)
        then start::(set_diff rest b)
    else set_diff rest b;;
(*let rec set_diff a b found = match a with*)
(*  |  [] -> found*)
(*  |  start::rest -> if (contain start b) then set_diff rest b found else set_diff rest b (start::found);;*)

let set_symdiff a b = set_union (set_diff a b) (set_diff b a);;

(*problem 5*)
(*This function is not possible to be written in Ocaml, because we don't have type 'set' in Ocaml, and when we represent
it as list and check whether or not s is contained by itself s, 'let self_member s = contain s s;;', it would raise
an error that the type variable 'a occurs inside 'a list, such that s is simultaneously considered as a list and an element,
which is invalid in Ocaml. So, it cannot be realized by Ocaml just like the logic in Russell's Paradox, such that a set
appears to be a member of itself if and only if it is not a member of itself. *)

(*problem 6*)
let rec computed_fixed_point eq f x =
    if eq (f x) x then x
  	else computed_fixed_point eq f (f x);;

(*problem 7*)
type ('nonterminal, 'terminal) symbol =
  	| N of 'nonterminal
  	| T of 'terminal;;

let rec entry_rules a b = match b with
  	| [] -> []
  	| start::rest ->
  	if (fst start) = a
  	    then start::(entry_rules a rest)
    else entry_rules a rest;;

(*find all the rules with nonterminal symbol*)
let rec find_nonterminal a = match a with
  	| [] -> []
  	| T first::rest -> find_nonterminal rest
  	| N first::rest -> (first)::(find_nonterminal rest);;

let rec find_useless a b c = match a with
  	| [] -> b
  	| first::rest -> let rules_from_a = entry_rules first b in
        let symbols_from_a = snd (List.split rules_from_a) in
        let nonterms = set_diff (find_nonterminal (List.flatten symbols_from_a)) c in
        find_useless (rest @ nonterms) (set_diff b rules_from_a) (first::c);;

let filter_reachable g =
    let useless = find_useless [(fst g)] (snd g) [] in
	(fst g, (set_diff (snd g) useless));;