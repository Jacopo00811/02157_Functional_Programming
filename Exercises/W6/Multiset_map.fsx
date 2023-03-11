type MultisetMap<'a when 'a : comparison> = Map<'a,int>;;

let ex_map_1 = Map.ofList [("b",3); ("a",5); ("d",1)];;
let ex_map_2 = Map.ofList [("a",3); ("b",4); ("c",2)];;


let inv ms = 
    if Map.forall (fun _ x -> x>0) ms then true 
    else failwith "This multiset has a negative number of occurences.";;


let insert e n ms = 
    if n<=0 then failwith "The value of occurences cannot be negative."
    else match Map.tryFind e ms with
         | None -> Map.add e n ms 
         | Some n' -> Map.add e (n+n') ms;;


let union(ms1, ms2) = Map.foldBack insert ms1 ms2;;

