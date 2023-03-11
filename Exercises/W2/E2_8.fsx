// Calculates the binomial coefficient
let bin(n, k) =
    let rec fact j =
        match j with
        | 0 -> 1
        | j when j>0 -> j*fact(j-1)
        | _ -> failwith "Negative argument"
    match (n,k) with
    | (n,0) -> 1
    | (n,k) -> fact(n)/(fact(k)*fact(n-k));;