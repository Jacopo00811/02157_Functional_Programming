(* 
let rec sum(p,xs) =
    match xs with
    | [] -> 0
    | x::xr  -> if p x then x+sum(p,xr) else sum(p,xr);; 
*)

// sum((fun x -> x>0),[1;-2;-4;2]);;

let sum p xs = List.fold (fun s x -> if p x then s+x else s) 0 xs

// Use anonymous function where s is the state = aka last iterationa