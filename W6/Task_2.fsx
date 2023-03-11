type Lid = string
type Flight = string
type Airport = string
type Route = (Flight * Airport) list
type LuggageCatalogue = (Lid * Route) list
type ArrivalCatalogue = (Airport * Lid list) list


// Tests 
let laggage_catalouge = [("DL 016-914", [("DL 189","ATL"); ("DL 124","BRU"); ("SN 733","CPH")]); ("SK 222-142", [("SK 208","ATL"); ("DL 124","BRU"); ("SK 122","JFK")])];;
let arrival_catalouge = [("ATL", ["DL 016-914"; "SK 222-142"]); ("BRU", ["DL 016-914"; "SK 222-142"]); ("JFK", ["SK 222-142"]); ("CPH", ["DL 016-914"])];;
let route = [("DL 189","ATL"); ("DL 124","BRU"); ("SN 733","CPH")];;
let luggage_catalouge_1 = [("DL 123-456", [("DL 189","ATL"); ("DL 124","BRU"); ("SN 733","CPH")]); ("SK 789-101", [("SK 208","ATL"); ("DL 124","BRT"); ("SK 122","JFK")])];;
let arriva_catalouge_1 = [("ATL", ["DL 016-914"; "SK 222-142"]); ("BRU", ["DL 016-914"; "SK 222-142"]); ("JFK", ["SK 222-142"]); ("CPH", ["DL 016-914"])];;
let cas1 = [("BRT", ["HERE"]); ("CPH", ["DL 016-914"]); ("JFK", ["SK 222-142"]); ("BRU", ["DL 016-914"; "SK 222-142"]); ("ATL", ["DL 016-914"; "SK 222-142"])];;
let cas2 = [("CPH", ["ECCOMI"]); ("CPH", ["I am HERE"; "DL 016-914"]); ("JFK", ["SK 222-142"]); ("BRU", ["DL 016-914"; "SK 222-142"]); ("ATL", ["DL 016-914"; "SK 222-142"])];;
let singleR = [("DL 189","BRT")];;
let multiR = [("DL 189","CPH"); ("DL 123", "BRT"); ("FG 200", "ATL")];;
let ac = [("ATL", ["DL 016-914"; "SK 222-142"]); ("BRU", ["DL 016-914"; "SK 222-142"]); ("JFK", ["SK 222-142"]); ("CPH", ["DL 016-914"])];;
// let acnew = [("ATL", ["DL 016-914"; "SK 222-142"]); ("BRU", ["DL 016-914"; "SK 222-142"; "AGGIUNTO PRIMA"]); ("JFK", ["SK 222-142"]); ("CPH", ["DL 016-914"])];;



// Functions

let rec findRoute((ID:Lid), (lc:LuggageCatalogue)) =
    match lc with
    | (Id, route)::_ when Id = ID -> route
    | (Id, route)::rest -> findRoute(ID, rest) 
    | [] -> failwith "The bag is not in the system";;


let rec inRoute (f:Flight) (route:Route) = 
    match route with
    | (fId, airpt)::_ when fId = f -> true
    | (fId, airpt)::rest -> inRoute f rest 
    | [] ->  false;;


let rec withFlight f = function
    | [] -> []
    | (lid,r)::lc when inRoute f r -> lid::withFlight f lc
    | _::lc -> withFlight f lc;;

// Helper functions for extend
let rec insert ac lid airpt acnew =
    match ac with  
    | (airpt1, bags)::rest1 when airpt = airpt1 -> insert rest1 lid airpt ((airpt1, lid::bags)::acnew) // might be wrong
    | (airpt1, bags)::rest1 when airpt <> airpt1-> insert rest1 lid airpt ((airpt1, bags)::acnew)
    | [] -> (airpt, [lid])::acnew
    | _ -> failwith "It is impossible to insert this bag"

let removeIfnecessary acnew =
    match acnew with 
    | (airpt1, bags)::rest ->
        let rec removeIfnecessaryRec airpt1 rest = 
            match rest with
            | (a, _)::tail when a = airpt1 ->  rest
            | (a, b)::tail -> (a, b)::removeIfnecessaryRec airpt1 tail 
            | [] -> [(airpt1, bags)]
        removeIfnecessaryRec airpt1 rest
    | _ -> acnew
  
let rec extend(lid:Lid, r:Route, ac:ArrivalCatalogue) = 
    match r with
    |  (f, airpt)::rest -> extend(lid, rest,removeIfnecessary (insert ac lid airpt []))
    | [] -> ac


let toArrivalCatalouge (lc:LuggageCatalogue) (ac:ArrivalCatalogue) = 
    List.foldBack (fun (lid, route) state -> extend(lid, route, state)) lc ac 



