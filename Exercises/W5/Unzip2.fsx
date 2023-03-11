let h (x,y) (xs,ys) = (x::xs,y::ys)
let unzip xys = List.foldBack h xys ([],[]);;