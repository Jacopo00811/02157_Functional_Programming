                                                    ////////////////////////////
                                                    // Jacopo Ceccuti s215158 //
                                                    ////////////////////////////


// PROBLEM 1
let rec f x = function
    | [] -> []
    | y::ys -> (x,y)::f x ys;;

let rec allPairs xs ys =
    match xs with
    | [] -> []
    | x::xrest -> f x ys @ allPairs xrest ys;;

// 1
(*
    The above declaration of f is not recursive because the last evaluation is the construct operator ":" and not the recursion call.
    f is not a tail call.
*)

// 2
let rec F x = function
    | (y::rest, res) -> F x (rest, (x, y)::res)
    | ([], res) -> List.rev res;;

//3
let rec Ff k x = function
    | [] -> k []
    | y::rest -> Ff (fun v -> k((x, y)::v)) x rest;;


// PROBLEM 2
// 1
(*
    ('a -> bool) -> ('a -> 'b)*('a -> 'b) -> 'a list -> 'b list
    The function f applies either, according to the result of the evaluation of g, the function h1 or h2 on every element of a list.
*)

// 2
let FF g (h1, h2) xs = List.foldBack (fun x state -> if g x then (h1 x)::state else (h2 x)::state) xs [];;

// 3
type A<'a> = 
    | D of 'a * bool
    | E of A<'a> * A<'a>;;

let rec g acc x = 
    match x with
    | E(y,z) -> g (g acc z) y
    | D(a,true) -> a::acc
    | _ -> acc;;

let h x = g [] x;;

let ex1 = E (D (["a"; "b"], false), D (["c"; "d"], true));;
let ex2 = D (["a"], false);;
let ex3 = E (E (D (["z"; "g"], true), D (["a"], true)), D (["c"; "d"], true));;

// 4 
(*
    Type of g:
    'a list -> A<'a> -> 'a list
*)

(*
    Type of h:
    A<'a> -> 'a list
    The function h filters all the branches of a tree of type A<'a> and creates a list of all the ('a*bool) where the boolean condition
    is "true".
    *)

// 5
(*
    No g is not tail recursive since the last evaluation is not a recursive call (in the second matching case it just constructs the 
    list).
*)

// 6
let rec G k x = 
    match x with
    | E(y,z) -> G (fun v -> G (fun p -> k(v@p)) z) y  //(G k y)@(G k z)
    | D(a,true) -> k [a]
    | _ -> k [];;

let H x = G id x;;


// PROBLEM 3
// 1 
let sq = seq [(1,2); (3,4); (5,6)];;

let flip sq = seq { for j in sq do
                    let (a, b) = j
                    yield! seq [(b, a)] }

// 2 
let dia n = seq { for j in [0 .. n] do
                  yield (j,(n-j)) };;

// 3 
let nat = Seq.initInfinite id;;

let allCoordinates = seq {for i in nat do
                            if i%2 = 0 then 
                                yield! dia i else
                                yield! flip (dia i) };;

let test = Seq.toList (Seq.take 20 allCoordinates);;

// PROBLEM 4
// 1
type T = Leaf of char | Branch of T*T;;

let t0 = Branch (Leaf 'a', Branch (Leaf 'b', Leaf 'c'));;
let t = Branch (Branch (Branch(Leaf 'e',Leaf 'd' ), Branch(Leaf 'c', Branch(Leaf 'o', Leaf 'l'))), Leaf 'a');;
let t1 = Branch (Leaf 'a', Branch (Leaf 'a', Leaf 'c'));; // Illegal 

let rec toListRec k t =
    match t with
    | Branch (n1, n2) -> toListRec (fun v -> toListRec (fun p -> k(v@p)) n2) n1
    | Leaf c -> k [c];;

let toList t = toListRec id t;;

// 2
let legal t =
    let res = toList t
    if List.length res >= 2 && Set.count(Set.ofList(res)) = List.length res then true else false;;

// 3
type Dir = 
    | L // go left
    | R;; // go right

type Code = Dir list;;

type CodingTable = Map<char, Code>;;

let cs = ['c';'a';'a';'b'];;
let ct = Map.ofList [('a', [L]); ('b', [R;L]); ('c', [R; R])];;

let rec encodeRec k ct cs =
    match cs with
    | c::rest -> match Map.tryFind c ct with
                 | None -> failwith "Impossible to create the code."
                 | Some c -> encodeRec (fun v -> k(c@v)) ct rest
    | [] -> k [];; 

let encode ct cs = encodeRec id ct cs;;

// 4
let rec ofTRec t route res =
    match t with
    | Leaf x -> (x, route)::res 
    | Branch (n1, n2) -> (ofTRec n1 (route@[L]) res)@(ofTRec n2 (route@[R]) res);;

let ofT t =  Map.ofList (ofTRec t [] []);;

// 5 


let rec findInC d c acc =
    let length = List.length d
    match List.take length c with
    | d' when (acc-length)<>length -> if d' = d then List.rev (List.take (List.length c - length) (List.rev c)) else findInC d (List.tail c) (acc+1)
    | _ -> failwith "Impossible to find" 


// Doesn't work
let firstCharOf t c =
    let ct = ofTRec t [] []
    let rec firstCharOfRec ct c res = 
        match ct with
        | (x, d)::rest -> firstCharOfRec rest (findInC d c 0) ((x, findInC d c 0)::res)
        | [] -> res
    firstCharOfRec ct c [];;


(*
let firstCharOf t c =
    let ct = ofTRec t [] []
    let firstCharOfRec ct c = 
        match ct with
        | (x, d)::rest -> (x, findInC d c 0)
        | _ -> failwith "Boh"
    firstCharOfRec ct c;;

*)
