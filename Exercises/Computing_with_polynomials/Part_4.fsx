type Poly = int list

type Degree =
    | MinusInf
    | Fin of int

let degree (xs: Poly) =
    match xs with
    | [] -> MinusInf
    | x::xtail -> Fin(List.length xtail)

let addM x y =
    match (x, y) with
    | (_,MinusInf) | (MinusInf,_) -> MinusInf
    | (Fin x, Fin y) -> Fin(x+y)
