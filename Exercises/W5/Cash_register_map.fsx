exception FindArticle;;

(* makebill: Register -> Purchase -> Bill *)
let rec makeBill reg = function
    | [] -> ([],0)
    | (np,ac)::pur ->
        match Map.tryFind ac reg with
        | None -> raise FindArticle
        | Some(aname,aprice) ->
            let tprice = np*aprice
            let (infos,sumbill) = makeBill reg pur
            ((np,aname,tprice)::infos, tprice+sumbill);;