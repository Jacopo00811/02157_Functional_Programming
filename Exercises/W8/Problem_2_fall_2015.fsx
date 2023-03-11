let rec g1 p = function
    | x::xs when p x -> x :: g1 p xs
    | _ -> [];;
(* 
    ('a -> bool) -> 'a list -> 'a list

    Filters the list according to the p condition and creates a new list with those filtered values until the first one that doesn't
    respect the condition.
*)

//////////////// Type and explain what it does
let rec g2 f h n x =
    match n with
    | _ when n<0 -> failwith "negative n is not allowed"
    | 0 -> x
    | n -> g2 h f (n-1) (f x);;
(*
    ('a -> 'a) -> ('a -> 'a) -> int -> 'a -> 'a
    The function counts the steps form a number n ????
*)

let rec G1 p = function 
    | (x::rest, res) when p x ->  G1 p (rest, res@[x])
    | (_, res) -> res;;
//G1 (fun x -> x>0) ([2;4;9;-12;0], []);;

let rec Gg1 p = function
    | x::rest when p x -> let v = Gg1 p rest
                          x::v
    | _ -> [];;
//Gg1 (fun x -> x>0) [2;4;9;-12;0];;

let rec gg1 p k = function
    | x::rest when p x -> gg1 p (fun v -> k(x::v)) rest
    | _ -> k [];;
//gg1 (fun x -> x>0) id [2;4;9;-12;0];;


(* 
    g2 is tail recursive because the recursive call is the last function application to be evaluated
    in the body of the declaration.
*)