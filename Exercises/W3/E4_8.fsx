let rec split xs =
    match xs with
    | [] -> ([],[])
    | x1::x2::xs -> let xr,yr = split xs
                    (x1::xr,x2::yr)
    | [_] -> ([],[])
                    