type Lid = string
type Flight = string
type Airport = string
type Route = (Flight * Airport) list
type LuggageCatalogue = (Lid * Route) list
type ArrivalCatalogue = (Airport * Lid list) list

let multiR = [("DL 189","CPH"); ("DL 123", "BRT"); ("FG 200", "ATL")];;

// Using the function exists it is easy to find if an element in the list route matches the passaed condition
//  (fun (x,y) -> x = f)
let inRoute (f:Flight) (route:Route) = List.exists (fun (x,y) -> x = f)  route;;

// Using fold we transverse the list and find the elements the match the if condition, 
// once found it is added to the list which is also the starting point and it's empty []
let withFlight f lc = List.fold (fun lids (lid,r) -> if inRoute f r then lid::lids else lids) [] lc