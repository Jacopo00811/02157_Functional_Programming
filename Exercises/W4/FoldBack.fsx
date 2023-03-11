List.foldBack (+) [1; 2; 3] 0;;
List.foldBack (-) [1; 2; 3] 0;; 


let absSum xs = List.foldBack (fun x a -> abs x + a) xs 0;;


let length xs = List.foldBack (fun _ n -> n+1) xs 0;;

// You can define List.map with List.foldBack
let map f xs = List.foldBack (fun x rs -> f x :: rs) xs [];;


// Insertion function
let insert x ys = 
    if List.contains x ys then ys else x::ys;;
// Deleates all the duplicates 
let distinct xs = List.foldBack insert xs [];; 
(* NB.
 List.foldBack: 'a -> 'state -> 'state 
 insert: 'a -> 'a list -> 'a list 
 They match perfectly *)
