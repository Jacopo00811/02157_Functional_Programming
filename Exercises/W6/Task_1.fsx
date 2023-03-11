let rec collect f = function
    | [] -> []
    | x::xs -> f x @ collect f xs;;

(*
    1. General type:
        (’a -> ’b list) -> ’a list -> ’b list
    2. collect (fun (a,b) -> [a..b]) [(1,3); (4,7); (8,8)];;
    3. What is the type used for collect in the expression?
        (int -> int list) -> int list -> int list
*)