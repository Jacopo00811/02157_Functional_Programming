(*
    let rec fact n=
    if n=0 then 1
    else n*fact(n-1);;
*)

// More complete 
let rec fact n =
    match n with
    | 0 -> 1
    | n when n>0 -> n*fact(n-1)
    | _ -> failwith "Negative argument";;

let rec optFact = function
    | 0 -> Some 1
    | n when n > 0 -> Some(n * Option.get(optFact(n-1)))
    | _ -> None;;