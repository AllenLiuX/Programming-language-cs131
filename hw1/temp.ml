let rec set_uniqunion a b = match a with
  | [] -> b
  | start::rest -> if (contain start b) then set_uniqunion rest b else start::(set_uniqunion rest b);;

let rec set_intersect a b = match a with
  |  [] -> []
  |  start::rest-> if(contain start b) then start::(set_intersect rest b) else set_intersect  rest b;;
