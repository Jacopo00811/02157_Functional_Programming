type Multiset<'a when 'a : equality> = ('a * int) list;;



let ex_set = [("b",3); ("a",5); ("d",1)];;



let rec invRec ms=
            match ms with
            | (e,_)::rest -> if List.exists (fun (e',_) -> e'=e) rest then false else invRec rest
            | [] -> true;;

let inv ms = 
    if List.forall (fun (_,x) -> x>0) ms && invRec ms then true 
    else failwith "This multiset has either: a negative number of occurences or a double element.";;


let rec insert e n ms = 
    if n<=0 then failwith "The value of occurences cannot be negative."
    else match ms with
         | [] -> [(e,n)]
         | (e',n')::rest when e'=e -> (e,n+n')::rest
         | (e',n')::rest -> (e',n')::(insert e n rest);;
 

let numberOf e ms = 
    match List.tryFind (fun (e',_) -> e'=e) ms with
    | Some (_,n) -> n
    | None -> failwith "The element is not in the multiset.";;


let delete e ms = 
    match List.tryFind (fun (e',_) -> e'=e) ms with
    | Some (e1,n1) -> List.filter (fun (e',_)-> e'<>e1) ms
    | None -> failwith "This element is not in the multiset therefore it is impossible to remove it.";;


let rec union(ms1, ms2) = 
    match ms1 with
    | [] -> ms2
    | (e,n)::rest -> union(rest, insert e n ms2);;