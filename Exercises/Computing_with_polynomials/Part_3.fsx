type Poly = int list
// If you want to see my solutions go in part 1 and 2. These are the well done and with the preserving invariant ones


// Part 3 moved 'isLegal' & 'prune' from part 2 to here
let isLegal p =
    match p with
    | [] -> true
    | xs -> (List.last p) <> 0

let prune p: Poly =
    let rec recPrune p: Poly =
        match p with
        | 0 :: xs -> recPrune xs
        | xs -> xs

    if (isLegal p) then p else p |> List.rev |> recPrune |> List.rev
// Part 3 end

let rec add (p1: Poly) (p2: Poly): Poly =
    match (p1, p2) with
    | ([], []) -> []
    | (xs, [])
    | ([], xs) -> xs
    | (x :: xs, y :: ys) -> x + y :: add xs ys
    |> prune // <-- Part 3

let mulC n (p: Poly): Poly =
    List.map (fun x -> x*n) p
    |> prune

let rec sub (p1: Poly) (p2: Poly): Poly =
    match (p1, p2) with
    | ([], []) -> []
    | (x :: xs, []) -> x :: xs
    | ([], x :: xs) -> -x :: sub [] xs
    | (x :: xs, y :: ys) -> x - y :: sub xs ys
    |> prune // <-- Part 3

let mulX (p: Poly): Poly = 0 :: p |> prune // <-- Part 3

let rec mul (p1: Poly) (p2: Poly): Poly =
    match p1 with
    | [] -> []
    | x :: xs -> add (mulC x p2) (mul xs (mulX p2))
    |> prune // <-- Part 3


let rec eval n (p: Poly) =
    match p with
    | [] -> 0
    | x :: xs -> x + eval n (mulC n xs)


// Part 2

// Moved 'isLegal' & 'prune' functions for Part 3

let toString p =
    let formatNum num power =
        match power with
        | 0 -> string num
        | 1 -> sprintf "%ix" num
        | n -> sprintf "%ix^%i" num power

    let rec toStringRec acc (p: Poly) =
        match p with
        | [] -> []
        | 0 :: xs -> toStringRec (acc + 1) xs
        | x :: xs -> (formatNum x acc) :: (toStringRec (acc + 1) xs)

    toStringRec 0 p |> String.concat " + "

let derivative (p: Poly): Poly =
    let rec deriveRec acc pRec =
        match pRec with
        | [] -> []
        | x :: xs -> (acc * x) :: (deriveRec (acc + 1) xs)

    if List.isEmpty p then p else
    p |> List.tail |> deriveRec 1

let compose (p1: Poly) (p2: Poly): Poly =
    let rec composeRec (p1r: Poly) (p2r: Poly): Poly =
        match p1r with
        | [] -> []
        | x :: xs -> add (mulC x p2r) (composeRec xs (mul p2r p2))

    if List.isEmpty p1 then p2 else add [ List.head p1 ] (composeRec (List.tail p1) p2)
    |> prune // <-- Part 3