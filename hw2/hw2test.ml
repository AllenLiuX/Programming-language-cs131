type my_nonterminals =
	| Sentence | NounPhrase | VerbPhrase | Noun | Verb | Adjective | Adverb

let my_grammar =
	(Sentence,
		function
		|Sentence ->
			[
				[N NounPhrase; N VerbPhrase]
			]
		|NounPhrase ->
			[
				[N Noun];
				[N Adjective; N Noun]
			]
		|VerbPhrase ->
			[
				[N Verb];
				[N Verb; N Adverb]
			]
		|Noun ->
			[
				[T"Dogs"];
				[T"Cats"]
			]
		|Verb ->
			[
				[T"run"];
				[T"walk"]
			]
		|Adjective ->
			[
				[T"Brown"];
				[T"White"]
			]
		|Adverb ->
			[
				[T"quickly"];
				[T"slowly"]
			])

(* Problem 5 *)
let make_matcher_test =
	((make_matcher my_grammar accept_all ["White"; "Dogs"; "run"; "slowly"]) = Some ["slowly"])

(* Problem 6 *)
let make_parser_test =
	((make_parser my_grammar ["White"; "Dogs"; "run"; "slowly"]) =
		Some (Node (Sentence,
            [Node (NounPhrase,
                [Node (Adjective,
                    [Leaf "White"]);
                Node (Noun,
                    [Leaf "Dogs"])]);
            Node (VerbPhrase,
                [Node (Verb,
                    [Leaf "run"]);
                Node (Adverb,
                    [Leaf "slowly"])])])))

