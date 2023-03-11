let inter xs ys = List.filter (fun x -> List.contains x ys) xs;; 
// Or using List.existes 
let inter' xs ys = List.filter (fun x -> List.exists (fun y -> x=y) ys) xs;;