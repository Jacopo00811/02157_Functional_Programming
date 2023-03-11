let a = 5;;
let f a = a + 1;;
let g b = (f b) + a;;

(* 
 a = 5 ALAWAYS
f(5)= 5+1 = 6
set b = 3 for example
g(3) = (f(3)) + 5  <---> (3+1)+5 = 9

*)