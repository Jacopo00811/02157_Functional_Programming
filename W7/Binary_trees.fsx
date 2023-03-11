// Type declaration of a tree
type Tree = | Lf
            | Br of Tree*int*Tree;;

(* Polymorphic type tree is defined in the following way
type Tree<’a> = | Lf 
                | Br of Tree<’a> * ’a * Tree<’a>;;    
*)


let tree = Br(Br(Br(Lf,2,Lf),7,Lf), 9, Br(Br(Lf,13,Lf),21,Br(Lf,25,Lf)));;


let rec inOrder = function
    | Lf -> []
    | Br(t1,j,t2) -> inOrder t1 @ [j] @ inOrder t2;;


let rec postOrder = function
    | Lf -> []
    | Br(t1,j,t2) -> postOrder t1 @ postOrder t2 @ [j];;


let rec preOrder = function
    | Lf -> []
    | Br(t1,j,t2) -> j:: preOrder t1 @ preOrder t2;;


let rec insert i = function
    | Lf -> Br(Lf,i,Lf)
    | Br(t1,j,t2) as tr -> // Layered pattern
        match compare i j with
        | 0 -> tr
        | n when n<0 -> Br(insert i t1 , j, t2)
        | _ -> Br(t1,j, insert i t2);;


let rec contains i = function
    | Lf -> false
    | Br(_,j,_) when i=j -> true
    | Br(t1,j,_) when i<j -> contains i t1
    | Br(_,j,t2) -> contains i t2;;