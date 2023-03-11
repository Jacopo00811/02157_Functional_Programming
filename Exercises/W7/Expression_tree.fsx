type Fexpr =
    | Const of float
    | X
    | Add of Fexpr * Fexpr
    | Sub of Fexpr * Fexpr
    | Mul of Fexpr * Fexpr
    | Div of Fexpr * Fexpr;;

let ex = Add(Sub(X, Const 2.0), Mul(Const 4.0, X));;

let rec compute x = function
    | Const r -> r
    | X -> x
    | Add(fe1,fe2) -> compute x fe1 + compute x fe2
    | Sub(fe1,fe2) -> compute x fe1 - compute x fe2
    | Mul(fe1,fe2) -> compute x fe1 * compute x fe2
    | Div(fe1,fe2) -> compute x fe1 / compute x fe2;;

(*
let rec substX e' e = 
    match e with 
    | x -> e' 
    | Const r -> Const r 
    | Add(fe1, fe2) -> Add(substX e' fe1, substX e' fe2)
*)


let rec D = function
    | Const _ -> Const 0.0
    | X -> Const 1.0
    | Add(fe1,fe2) -> Add(D fe1,D fe2)
    | Sub(fe1,fe2) -> Sub(D fe1,D fe2)
    | Mul(fe1,fe2) -> Add(Mul(D fe1,fe2),Mul(fe1,D fe2))
    | Div(fe1,fe2) -> Div(Sub(Mul(D fe1,fe2),Mul(fe1,D fe2)), Mul(fe2,fe2));;