let rec f g = function
| [] -> []
| x::xs -> g x :: f (fun y -> g(g y)) xs;;

 // f: g: ('a -> 'a) ->  'a list -> 'a list