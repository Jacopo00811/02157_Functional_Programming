let rec unzip = function
    | []          -> ([],[])
    | (x,y)::rest -> let (xs,ys) = unzip rest
                     (x::xs,y::ys);;


let rec Unzip = function
    | [] -> ([],[])
    | (x,y)::rest ->
        let (xs,ys) = Unzip rest
        (x::xs,y::ys);;