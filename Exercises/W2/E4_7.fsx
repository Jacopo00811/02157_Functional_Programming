let rec multiplicity x xs = 
    match xs with
    | y::xs when x=y -> 1 +  multiplicity x xs
    | y::xs -> multiplicity x xs
    | [] -> 0;;