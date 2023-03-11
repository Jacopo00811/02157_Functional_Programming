let rec (@) xs ys =
    match xs with
    | [] -> ys
    | x::xtail -> x::(xtail @ ys);;