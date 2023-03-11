let rec zip (xs,ys) = 
    match (xs,ys) with
    | ([],[]) -> []
    | (_,[]) -> []
    | ([],_) -> []
    | (x::xrest,y::yrest) -> (x,y)::zip(xrest, yrest);;
                             
                             
    
    