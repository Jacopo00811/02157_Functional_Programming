// Repetes the string n times
let rec pow(s, n) =
    match (s, n) with
    | (_, 0) -> ""
    | (_, 1) -> s
    | (s, n) -> s + pow(s, n-1)