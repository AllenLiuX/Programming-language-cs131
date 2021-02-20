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

let terminal_frag = ["Jerry"; "wants"; "a"; "gift"];;
let make_matcher_test = ((make_matcher english_grammar accept_all ["Jerry"; "wants"; "a"; "gift"]) = Some ["a"; "gift"])
let make_parser_test1 =
  ((make_parser english_grammar terminal_frag) =
   Some
   (Node (S,
     [Node (NP, [Node (PN, [Leaf "Jerry"])]);
      Node (VP,
       [Node (V, [Leaf "wants"]);
        Node (NP, [Node (A, [Leaf "a"]); Node (Nn, [Leaf "gift"])])])])));;

(*Problem 6*)
let terminal_frag = ["Jerry"; "wants"; "a"; "gift"];;
let parse = make_parser english_grammar terminal_frag;;

(*let make_matcher_test = ((make_matcher english_grammar accept_all ["Jerry"; "wants"; "a"; "gift"]) = Some ["a"; "gift"])*)
(*let terminal_frag = ["Jerry"; "wants"; "a"; "gift"]*)
(*make_parser english_grammar terminal_frag*)



type nonterminals =
| Term | Binop | Expr | Lvalue | Incrop | Num

let ambigrammar =
(Expr,
   function
     | Expr ->
         [[N Term; N Binop; N Expr];
          [N Term]]
     | Term ->
         [[N Num];
          [N Lvalue];
          [N Incrop; N Lvalue];
          [N Lvalue; N Incrop];
          [T"("; N Expr; T")"]]
     | Lvalue ->
         [[T"$"; N Expr]]
     | Incrop ->
         [[T"++"]; [T"--"]]
     | Binop ->
         [[]; [T"+"]; [T"-"]]
     | Num ->
         [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
          [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]]);;

let terminal_frag = ["1"; "++"; "+"; "2"]
let parse = make_parser ambigrammar terminal_frag;;

let terminal_frag =  ["$"; "1"; "++"; "+"; "$"; "2"]
let parse = make_parser ambigrammar terminal_frag;;


let terminal_frag2 = ["$"; "1"; "1"]
let parse2 = make_parser ambigrammar terminal_frag2;;

let terminal_frag3 = ["$"; "1"; "++"; "$"; "2"]
let parse3 = make_parser ambigrammar terminal_frag3;;

make_matcher english_grammar accept_all terminal_frag3;;
make_matcher english_grammar accept_all terminal_frag;;