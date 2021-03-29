let rec product k = function
    | [] -> k 1
    | 0::tl -> k 0
    | hd::tl ->
        let k' x = k (hd * x) in
        product k' tl

let m a = a
let res = product m [1;2;3;4;5];;

let res2 = product (fun x->x) [1;2;3;4;5];;