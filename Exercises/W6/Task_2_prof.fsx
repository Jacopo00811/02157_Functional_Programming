type Lid = string
type Flight = string
type Airport = string
type Route = (Flight * Airport) list
type LuggageCatalogue = (Lid * Route) list
type ArrivalCatalogue = (Airport * Lid list) list

let rec findRoute(lid, lc) =
    match lc with
    | (lid1,r1)::_ when lid1=lid -> r1
    | _::lcrest -> findRoute(lid,lcrest)
    | _ -> failwith (lid + " is not found")


let rec inRoute f = function
    | (f1,_)::r -> f=f1 || inRoute f r
    | [] -> false


let rec withFlight f = function
    | [] -> []
    | (lid,r)::lc when inRoute f r -> lid::withFlight f lc
    | _::lc -> withFlight f lc


let rec insert lid airp = function
    | [] -> [(airp, [lid])]
    | (airp1,ls)::rest when airp1=airp -> (airp1,lid::ls)::rest
    | pair::rest -> pair::insert lid airp rest

let rec extend(lid, r, ac) =
    match r with
    | [] -> ac
    | (f,airp)::r1 -> extend (lid, r1, insert lid airp ac)