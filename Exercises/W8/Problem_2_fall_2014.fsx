let multTable n = 
    Seq.tail(Seq.take 11 (Seq.initInfinite (fun i -> i*n)));;



let tableOf m n f = 
    seq { for i in [1..m] do 
            for j in [1..n] do 
                yield (i, j, f i j) }

//let p = Seq.toList (tableOf 3 4 (+));;



let seqString item =
    let rec repeat items item = 
        seq { yield items + item // adds it to the seq.
              yield! repeat (items+item) item }
    repeat "" item;;






let rec f i = function
    | [] -> []
    | x::xs -> (x+i)::f (i*i) xs;;
(*
    'a -> 'b list -> 'c list

    The function gets a list and a starting elemnt i. At each element of the list is i is added and the square of i is evaluated for
    for the next iteration. So basically adds the i^(2*n) (with n that goes from 1 to an unknown value) to each elements of the list.  
*)

let rec F i = function
    | (x::rest, res) -> F (i*i) (rest, res@[x+i])
    | (_, res) -> res;;
//F 1 ([1;2;3], []);;

let rec ff i k = function
    | [] -> k []
    | x::rest -> ff (i*i) (fun v -> k((x+i)::v)) rest;;
//ff 1 id [1;2;3];;

let rec Ff i = function
    | x::rest -> let v = Ff (i*i) rest
                 (x+i)::v
    | [] -> [];;
//Ff 1 [1;2;3];;