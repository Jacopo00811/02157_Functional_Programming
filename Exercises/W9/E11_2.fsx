let rec fact n =
    match n with
    | 0 -> 1
    | n when n>0 -> n*fact(n-1)
    | _ -> failwith "Negative argument";;


let even = Seq.initInfinite (fun x -> fact x);;
Seq.toList (Seq.take 15 even);;