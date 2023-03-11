type Poly = int list

let rec add xs ys =
    match (xs,ys) with
    | ([],[]) -> []
    | (xs,[]) -> xs:Poly
    | ([],ys) -> ys
    | (x::xtail,y::ytail) -> (x+y)::add xtail ytail;;


let rec mulC k xs =
    match(k,xs) with
    | (_,[])-> []
    | (k:int,x::xtail) -> (k*x)::mulC k xtail;;


let rec sub xs ys =  
    match (xs,ys) with
    | ([],[]) -> []
    | (xs,[]) -> xs 
    | ([],ys)-> ys 
    | (x::xtail,y::ytail) -> (x-y)::sub xtail ytail;;


let mulX xs =
    match xs with
    | [] -> []
    | xs -> 0::xs;;


let rec mul xs ys =
    match xs with
    | [] -> []
    | x::xtail -> add (mulC x ys) (mulX (mul xtail ys));;


let rec evalh x e ps =
    match ps with
    | [] -> 0
    | p::ps -> p*e+evalh x (e*x) ps

let eval x ps = 
    evalh x 1 ps;; 