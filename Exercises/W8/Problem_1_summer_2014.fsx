let rec f n = function 
    | 0 -> 1
    | k when k>0 -> n * (f n (k-1))
    | _ -> failwith "illegal argument";;
//f 2 3;; 
(*
    'a -> 'a -> 'a
    The function computes n*n*n... k times.
*)


let rec g p f = function
    | [] -> []
    | x::xs when p x -> f x :: g p f xs
    | _::xs -> g p f xs;;
//g (fun x -> x>0) (fun y -> y+1) [1;2;3;-4;5];; 
(*
    ('a -> bool) -> ('a -> 'b) -> 'a list -> 'b list
    The function computes applies f to all the elements of the list that satisfy the condition p and creates a list of them.
*)


type T = 
    | A of int
    | B of string
    | C of T*T;;

let rec h = function
    | A n -> string n
    | B s -> s
    | C(t1,t2) -> h t1 + h t2;;

let test = C(A(2), C(B("hey"), A(4)));;
//h test;; 
(*
    T -> string
    The function prints as a string all the type avaible in the descrtiption T above (in order from left to right). 
*)



let rec F n j = function 
    | 0 -> j 1
    | k when k>0 -> F n (fun v -> j(n*v)) (k-1)
    | _ -> failwith "illegal argument";;
//F 2 id 3;;


let sq = Seq.initInfinite (fun i -> 3*i);;
(*
    sq<int>
    It is the infinite sequence of numbers that are multiple of 3
*)

let k j = seq {for i in sq do
                yield (i,i-j) };;
(*
    seq<int*int>
    For a sequence made up by tuples, it subtracts j from the second element of the tuple.
*)

let xs = Seq.toList (Seq.take 4 sq);;
(*
    int list
*)

let ys = Seq.toList (Seq.take 4 (k 2));;
(*
    int list
*)