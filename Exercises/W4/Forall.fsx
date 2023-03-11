List.forall (fun x -> x>=2) [1; 3; 1; 4];;


let disjoint xs ys = List.forall (fun x-> not (List.contains x ys)) xs;;
 // Or using List.existes
let disjoint' xs ys = List.forall (fun x-> not (List.exists (fun y-> y=x) ys)) xs;;

// Example on map coloring in the MapColoring.fsx file

// let prop1 m = let cs = countries m
//              List.forall (fun(c1,c2) -> List.contains c1 cs && c2 cs)