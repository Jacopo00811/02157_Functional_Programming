let Grater_than x = List.exists (fun x -> x>=2) [1; 3; 1; 4];;

let contains x ys = List.exists (fun y -> y=x) ys;;