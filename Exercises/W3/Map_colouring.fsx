type Country = string;;
type Map = (Country * Country) list;;
type Colour = Country list;;
type Colouring = Colour list;;

let rec isMember x = function
    | y::ys -> x=y || (isMember x ys)
    | [] -> false;;

let exMap = [("a","b"); ("c","d"); ("d","a")];;


let areNb m c1 c2 =
    isMember (c1,c2) m || isMember (c2,c1) m;;


let rec canBeExtBy m col c =
    match col with
    | [] -> true
    | c'::col' -> not(areNb m c' c) && canBeExtBy m col' c;;


let rec extColouring m cols c =
    match cols with
    | [] -> [[c]]
    | col::cols' -> if canBeExtBy m col c
                    then (c::col)::cols'
                    else col::extColouring m cols' c;;


let addElem x ys = if isMember x ys then ys else x::ys;;


let rec countries = function
    | [] -> []
    | (c1,c2)::m -> addElem c1 (addElem c2 (countries m));;


let rec colCntrs m = function
    | [] -> []
    | c::cs -> extColouring m (colCntrs m cs) c;;


let colMap m = colCntrs m (countries m);;