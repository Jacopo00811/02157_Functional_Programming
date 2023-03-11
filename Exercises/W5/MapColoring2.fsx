let areNb c1 c2 m = Set.exists (fun pair -> pair = (c1,c2) || pair = (c2,c1)) m;;

let canBeExtBy m col c = Set.forall (fun c' -> not (areNb c c' m)) col;;

let rec extColouring m cols c =
    if Set.isEmpty cols
    then Set.singleton (Set.singleton c)
    else let col = Set.minElement cols
         let cols' = Set.remove col cols
         if canBeExtBy m col c
         then Set.add (Set.add c col) cols'
         else Set.add col (extColouring m cols' c);;


let countries m = Set.fold (fun s (c1,c2) -> Set.add c1 (Set.add c2 s)) Set.empty m;;


let colCntrs m cs = Set.fold (extColouring m) Set.empty cs;;

let colMap m = colCntrs m (countries m);;


let exMap = Set.ofList [("a","b"); ("c","d"); ("d","a")];;
colMap exMap;;