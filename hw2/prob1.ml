let nonzero a b = match b with
    | 0 -> 0
    | _ -> 1;;

let rec a b c  = b (a b) c;;

let exp = a nonzero;;

exp 4;;
(*exp 0;;*)

let exp = a nonzero 5;;