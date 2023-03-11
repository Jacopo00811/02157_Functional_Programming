(* This function takes a predicate and a list as input and in the firat step gives back a lits. When you
extract the first element of the list it checks the predicate and if it is true create a list in output with the elemnet
at the beginnig, otherwise it appends the element at the end *)

let rec p q = function
    | [] -> []
    | x::xs -> let ys = p q xs
               if q x then x::ys else ys@[x];;

// p (fun x->x<=5) [1;2;4;7;9;10];;