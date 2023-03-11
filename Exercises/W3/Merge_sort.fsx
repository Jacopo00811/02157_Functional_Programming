#load "E4_11.fsx";;

let rec insert (y:int) xs =
    match xs with
    | [] -> [y] 
    | x::_ when y<=x -> y::xs
    | x::rest -> x::insert y rest;;


let rec merge(xs ,ys) =
    match xs with
    | [] -> ys
    | b::xtail -> 
            let res = insert b ys 
            merge(xtail,res);;

   

let split xs=

    let rec splitRec acc xs (ys, zs)=
        match xs with
        | x::xtail when acc % 2 = 0 -> splitRec (acc+1) xtail ((insert x ys), zs)
        | x::xtail when acc % 2 <> 0 -> splitRec (acc+1) xtail (ys, (insert x zs))
        | _ -> (ys, zs)
    
    splitRec 0 xs ([], []);;


let sort xs = 

    let rec sortRec (ys, zs) (yss, zss) =
        match (ys, zs) with
        | ([], []) -> (yss, zss)
        | (y::ytail,z::ztail) -> sortRec (ytail, ztail) (insert y yss, insert z zss)
        | (_,_) -> (yss,zss)     
    merge (sortRec (split xs) ([], []));;




// randomList n range generates a random list of length n containing integers between 0 and range.

let randomList n range = let rand = let gen = new System.Random()
                                    (fun max -> gen.Next(max))
                         List.init n (fun _ -> rand range);;


