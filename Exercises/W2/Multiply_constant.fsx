let rec mulC k = function
    | [] -> []
    | x::tail -> k*x::mulC k tail;;