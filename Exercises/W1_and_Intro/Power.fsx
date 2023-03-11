let rec power(x,n) =
    match (x,n) with
    | (_,0) -> 1.0
    | (x,n) -> x*power(x,n-1);;
    
// High-order version of power 
let rec powerr x n = 
    match n with
    | 0 -> 1.0
    | _ -> x * powerr x (n-1);;

 // Also can be written as
let rec powerrr x =
    function
    | 0 -> 1.0
    | n -> x*powerrr x (n-1);;