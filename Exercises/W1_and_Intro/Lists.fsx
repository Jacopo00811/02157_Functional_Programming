(* Types of list *)
[2;3;6];;

["a";"b";"c"];;

[sin;cos];;
// Constructers of lists 
1::(2::[]);;

1::[2;3];;

/// Recursion on a list 
let rec suml xs=
    match xs with
    | [] -> 0
    | x::tail -> x + suml tail;;
