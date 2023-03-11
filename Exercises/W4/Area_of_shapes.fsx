type Shape = | Circle of float
             | Square of float
             | Triangle of float*float*float;;


(* let area = function
    | Circle r -> System.Math.PI * r * r
    | Square a -> a * a
    | Triangle(a,b,c) ->
            let s = (a + b + c)/2.0
            sqrt(s*(s-a)*(s-b)*(s-c));; *)


let isShape = function
    | Circle r -> r > 0.0
    | Square a -> a > 0.0
    | Triangle(a,b,c) ->
        a > 0.0 && b > 0.0 && c > 0.0
        && a < b + c && b < c + a && c < a + b;;


let area x =
    if not (isShape x)
    then failwith "not a legal shape" 
    else match x with
         | Circle r -> System.Math.PI * r * r
         | Square a -> a * a
         | Triangle(a,b,c) ->
            let s = (a + b + c)/2.0
            sqrt(s*(s-a)*(s-b)*(s-c));;