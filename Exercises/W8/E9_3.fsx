let rec sum = function
    | (m, 0) -> m
    | (m, n) -> sum(m, n-1)+(m+n);;
//sum (3,2);;

let rec Sum = function
    | (m, n, res) when n>=0 -> Sum (m, n-1, res+(m+n))
    | (_, _, res) -> res;;
//Sum (3,2,0);;

// Not really tail-recursive but kinda
let rec Ssum k = function
    | (m, 0) -> k m
    | (m, n) -> m+Ssum (fun v -> k(m+v)) (m, n-1);;
//Ssum id (3,2);;