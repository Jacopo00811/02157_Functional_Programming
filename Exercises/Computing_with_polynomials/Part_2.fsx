let isLegal xs = if List.last xs = 0 then false else true;;


let rec prune xs =
    match xs with
    | [] -> []
    | xs when List.last xs=0 -> let reved = List.rev xs
                                match reved with
                                | x::revedtail -> prune(List.rev revedtail)
                                | [] -> []
    | _ -> xs;; 


let toString xs = 
    let format num pow =
        match pow with 
        | 0 -> string num
        | 1 -> sprintf "%ix" num 
        | p -> sprintf "%ix^%i" num pow
    
    let rec toStringRec exp xs=
        match xs with
        | [] -> []
        | 0::xtail -> toStringRec (exp+1) xtail
        | x::xtail -> (format x exp) :: toStringRec(exp+1) xtail
    
    toStringRec 0 xs |> String.concat " + ";;


let derivative xs =
    let rec derivativeRec acc xs = 
        match xs with
        | x::xtail when acc = 0 -> 0::derivativeRec (acc+1) xtail
        | x::xtail -> (x*acc)::derivativeRec (acc+1) xtail
        | [] -> []

    derivativeRec 0 xs;;

// To define the compose function we need "add", "mulC", "mulX" and "mul" 

let rec add xs ys =
    match (xs,ys) with
    | ([],[]) -> []
    | (xs,[]) -> xs
    | ([],ys) -> ys
    | (x::xtail,y::ytail) -> (x+y)::add xtail ytail;;


let rec mulC k xs =
    match(k,xs) with
    | (_,[])-> []
    | (k:int,x::xtail) -> (k*x)::mulC k xtail;;

// let rec MulC k xs = List.map (fun x -> k*x) xs

let mulX xs =
    match xs with
    | [] -> []
    | xs -> 0::xs;;


let rec mul xs ys =
    match xs with
    | [] -> []
    | x::xtail -> add (mulC x ys) (mulX (mul xtail ys));;



let compose p1 p2 =
    let rec composeRec p1r p2r =
        match p1r with
        | [] -> []
        | x :: xs -> add (mulC x p2r) (composeRec xs (mul p2r p2))

    if List.isEmpty p1 then p2 else add [ List.head p1 ] (composeRec (List.tail p1) p2)
    |> prune // <-- Part 3

