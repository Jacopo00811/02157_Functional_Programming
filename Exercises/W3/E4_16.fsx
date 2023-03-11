(* Getsas imput an integer and a list of int. and gives back the sum of that integer plus the first element of the list.
On the next iteration it decrements the value of the integer and keeps summing it to the second element of the list,
and so on... *)
let rec f = function
    | (x, []) -> []
    | (x, y::ys) -> (x+y)::f(x-1, ys);;


(* This function gets in input a list and extracts the first touple, the adds the touple and its inverse to the list *)
let rec g = function
    | [] -> []
    | (x,y)::s -> (x,y)::(y,x)::g s;;


(* This function gets a list as input and appends a revesresd list to the given one *)
let rec h = function
    | [] -> []
    | x::xs -> x::(h xs)@[x];;