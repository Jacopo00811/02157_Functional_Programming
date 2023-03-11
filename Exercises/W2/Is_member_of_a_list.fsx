let rec isMember x = function
    | y::ys -> x=y || (isMember x ys)
    | [] -> false;;