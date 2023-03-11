let rec sum(p,xs) =
    match xs with
    | [] -> 0
    | x::xr  -> if p x then x+sum(p,xr) else sum(p,xr);; 

// sum((fun x -> x>0),[1;-2;-4;2]);;