let rec length = function
    | x::rest -> let v = length rest
                 v+1
    | _ -> 0;;
