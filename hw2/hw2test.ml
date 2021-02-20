(*Problem 5*)
let accept_all string = Some string
let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x

type english_nonterminals =
  | S | NP | VP | A | Nn | PN | V

let english_grammar =
(S,
function
 | S ->
    [[N NP; N VP]]
 | VP ->
    [[N V]; [N V; N NP]]
 | NP ->
    [[N A; N Nn]; [N PN]]
 | A ->
    [[T"a"]; [T"the"]]
 | Nn ->
    [[T"cat"]; [T"gift"]; [T "book"]; [T "house"]; [T "map"]]
 | PN ->
    [[T"Jack"]; [T"Vincent"]; [T"Jerry"]]
 | V ->
    [[T "sees"]; [T "have"]; [T "is"]; [T "wants"]; [T "takes"]]
)

let make_matcher_test = ((make_matcher english_grammar accept_all ["Jerry"; "wants"; "a"; "gift"]) = Some ["a"; "gift"])

(*Problem 6*)
let terminal_frag = ["Jerry"; "wants"; "a"; "gift"]

let make_parser_test1 =
  ((make_parser english_grammar terminal_frag) =
   Some
   (Node (S,
     [Node (NP, [Node (PN, [Leaf "Jerry"])]);
      Node (VP,
       [Node (V, [Leaf "wants"]);
        Node (NP, [Node (A, [Leaf "a"]); Node (Nn, [Leaf "gift"])])])])));;

let make_parser_test2 =
    parse_tree_leaves
   (Node (S,
     [Node (NP, [Node (PN, [Leaf "Jerry"])]);
      Node (VP,
       [Node (V, [Leaf "wants"]);
        Node (NP, [Node (A, [Leaf "a"]); Node (Nn, [Leaf "gift"])])])])) = terminal_frag;;