 let h (x,y) (xs,ys) = (x::xs,y::ys);;
let unzip xys = List.foldBack h xys ([],[]);;

let unmixMap f g xys =
    let (xs,ys) = unzip xys 
    (List.map f xs, List.map g ys)



let rec zip xs ys =
    match xs, ys with
    | (x::xtail), (y :: ytail) -> (x,y)::zip xtail ytail
    | [], (y::ytail) -> failwith "List do not have the same length" 
    | (x::xtail), [] -> failwith "List do not have the same length" 
    | [], [] ->  []

let mixMap f xs ys =
    let zs = zip xs ys
    List.map f zs

// mixMap (fun (x,y) -> (x*x,y*y)) [3;2] [1;4];;
// unmixMap (fun x -> x*x) (fun y -> y-1) [(2,1);(3,2)];; 