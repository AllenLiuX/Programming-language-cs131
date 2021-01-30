let my_subset_test0 = subset [] []
let my_subset_test1 = subset [] [1; 2]
let my_subset_test2 = subset [1; 1; 2; 3; 3] [1; 2; 3]
let my_subset_test3 = not (subset [1; 2; 3; 4] [1; 2; 3])

let my_equal_sets_test0 = equal_sets [] []
let my_equal_sets_test1 = not (equal_sets [1; 2; 3] [1; 2 ;3; 4])
let my_equal_sets_test2 = equal_sets [1; 2] [2; 1]

let my_set_union_test0 = equal_sets (set_union [1; 2] [2; 3; 4]) [1; 2; 3; 4]
let my_set_union_test1 = equal_sets (set_union [2; 3] [1; 4]) [1; 2; 3; 4]

let my_set_symdiff_test0 = equal_sets (set_symdiff [1; 2; 3] [1; 2; 3]) []
let my_set_symdiff_test1 = equal_sets (set_symdiff [1; 2; 4] [1; 3]) [2; 3; 4]
let my_set_symdiff_test2 = equal_sets (set_symdiff [1] [2; 3]) [1; 2; 3]

let my_computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> x / 3) 3333 = 0
let my_computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> x * 2) 0 = 0

type nonterminals =
  | A | B | C | D | E

let grammar =
  A,
  [ A, [N B];
    A, [N C];
    A, [N D];
    A, [T "aa"];
    B, [T "bb"];
    B, [T "cc"];
    C, [T "dd"];
    C, [T "ee"];
    D, [N E];
    D, [T "ff"; N E];
    D, [T "gg"; T "cc"; N B];
    D, [T "gg"; N C];
    E, [T "hh"; N B]]

let my_filter_reachable_test0 = (filter_reachable grammar = grammar)
let my_filter_reachable_test1 =
  (filter_reachable (D, snd grammar) =
  (D, [B, [T "bb"];
        B, [T "cc"];
        C, [T "dd"];
        C, [T "ee"];
        D, [N E];
        D, [T "ff"; N E];
        D, [T "gg"; T "cc"; N B];
        D, [T "gg"; N C];
        E, [T "hh"; N B]]))
let my_filter_reachable_test2 =
  (filter_reachable (C, snd grammar) =
  (C, [C, [T "dd"];
       C, [T "ee"]]))