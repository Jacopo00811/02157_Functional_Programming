let h x (s, p) = (s+x, p*x)
let rec sumprod xs =
    match xs with
    | [] -> (0,1)
    | x::tail -> h x (sumprod tail);;

let rec SumProd = function
    | [] -> (0,1)
    | x::tail -> let (rSum,rProd) = SumProd tail
                 (x+rSum,x*rProd);;