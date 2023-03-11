let rec findPosA p x = function // Using the option type
    | y::_ when x=y -> Some p
    | _::ys -> findPosA (p+1) x ys
    | [] -> None;;