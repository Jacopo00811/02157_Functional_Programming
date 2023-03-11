let rec count(xs:int list,x:int) = 
    match xs with
    | y::xs when x=y -> 1 + count(xs,x)
    | y::xs -> count(xs,x)
    | [] -> 0;;
    

let rec insert y xs =
    match xs with
    | [] -> [y] 
    | x::_ when y<=x -> y::xs
    | x::rest -> x::insert y rest;;


let rec mem list x = 
    match list with
    | [] -> false
    | head :: tail -> if x = head then true else mem tail x

let rec intersect(list2,list1) = 
    match list1 with
    | [] -> []
    | head :: tail -> 
        let rest = intersect(list2,tail)
        if mem list2 head then insert head rest
        else rest


let plus(xs,ys) = List.sort(xs@ys)


let rec remove y xs=
    match xs with
    | []                             -> []:int list
    | x::tail when x=y -> remove y tail
    | x::tail          -> x::remove y tail;;

let rec remove_one y xs =
    match xs with 
    | [] -> []
    | x::xtail when x=y -> xtail
    | x::xtail -> x::remove_one y xtail;;


let minus(xs,ys) =
    let inter = intersect(xs,ys)
    let rec iter inter xs =
        match inter with
        | [] -> xs
        | i::intertail when mem xs i ->
                let res = remove_one i xs
                iter intertail res
        | i::intertail -> iter intertail xs
    iter inter xs;;
