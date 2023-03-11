let rec insert x ys =
    match ys with
    | [] -> [x]
    | y::_ when x <= y -> x::ys
    | y::ytail -> y::insert x ytail

let rec sort xs =
    match xs with
    | [] -> []
    | x::xtail -> insert x (sort xtail);;