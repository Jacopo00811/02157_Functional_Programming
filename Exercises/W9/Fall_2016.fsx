// PROBLEM 1
type Name = string
type Event = string
type Point = int
type Score = Name * Event * Point
type Scoreboard = Score list

let sb = [("Joe", "June Fishing", 35); ("Peter", "May Fishing", 30); ("Joe", "May Fishing", 28); ("Paul", "June Fishing", 28)];;

let rec inv = function
    | [] -> true
    | [(_, _, p)] -> p>=0
    | (_, _, p)::(n1, e1, p1)::sb -> p>=p1 && inv ((n1, e1, p1)::sb);;

let rec insert s sb = 
    let (per, comp, p) = s
    match sb with
    | [] -> [s]
    | (_, _, p')::_ when p>=p' -> s::sb
    | s'::rest -> s'::insert s rest;;

// SOLUTION
let rec Insert (_, _, p as s) = function
    | [] -> [s]
    | _, _, p' as s'::sb when p>p' -> s::s'::sb
    | s'::sb -> s'::Insert s sb;;

// Could also have done it with a continuation based approach
let get(n, sb) = 
    let rec getRec(n, sb, res) =
        match sb with
        | (per, comp, p)::rest when n=per -> getRec(n, rest, (comp, p)::res) 
        | _::rest -> getRec(n, rest, res)
        | [] -> List.rev res
    getRec(n, sb, []);;

// SOLUTION 
let rec Get (n, sb) =
    match sb with 
    | [] -> []
    | (n1, e1, p1)::sb1 when n=n1 -> (e1, p1)::Get (n, sb1)
    | _::sb1 -> Get (n, sb1);;

let rec topRec k sb acc res =
    match sb with
    | s::stail when acc<k -> topRec k stail (acc+1) (s::res)
    | s::stail -> topRec k stail acc res
    | [] -> List.rev res;;

let top k sb =
    if k<0 || List.length sb < k then None 
    else Some (topRec k sb 0 []);;

// SOLUTION 
let rec Top k = function
    | _ when k=0 -> Some []
    | [] -> None
    | s::sb -> 
        match top (k-1) sb with
        | None -> None
        | Some res -> Some (s::res);;


// PROBLEM 2
let rec replace a b xs =
    match xs with
    | x::rest -> let v = replace a b rest
                 if x=a then b::v else x::v
    | [] -> [];;
// replace 2 7 [1; 2; 3; 2; 4];;

// Solution
let rec replace1 a b = function
    | [] -> []
    | x::rest when x=a -> b::(replace1 a b rest)
    | x::rest -> x::(replace1 a b rest)
(*
    'a -> 'a -> 'a list -> 'a list
    My function is not tail recursive because the last operation is not the recursive call, so below 
    there is the version with an accumulating parameter.
*)

let rec Replace a b (xs, res)= 
    match (xs, res) with
    | (x::rest, res) when x=a -> Replace a b (rest, res@[b]) 
    | (x::rest, res) -> Replace a b (rest, res@[x])
    | (_, res) -> res;;
// Replace 2 7 ([1;2;3;2;4], []);;


// PROBLEM 3
let pos = Seq.initInfinite (fun i -> i+1) ;;
(*
    seq<int>
    Sequence of ascending numbers.
*)

let seq1 = seq { yield (0,0)
                 for i in pos do
                 yield (i,i)
                 yield (-i,-i) }
(*
    seq<int * int>
    Sequence of tuple of the type (i, i) and (-i, i) form 1 to infinity.
*)

let val1 = Seq.take 5 seq1;;
(*
    seq<int * int>
    Sequence of tuple of the type (i, i) and (-i, i) form 1 to 4.
*)

let nat = Seq.initInfinite id;;

let seq2 = seq { for i in nat do
                 yield (i,0)
                 for j in [1 .. i] do
                 yield (i,j) }
(*
    seq<int * int>
    Does all the evaluation of (i, 0) to (i, j) (where i=j in the last one) and then increases i.
*)

let val2 = Seq.toList(Seq.take 10 seq2);;
(*
    seq<int * int>
*)


// PROBLEM 4
type Tree<'a,'b> = 
    | A of 'a 
    | B of 'b
    | Node of Tree<'a,'b>*Tree<'a,'b>;;

let tree = Node(Node(A true, B [4;5;6]), Node (A false, Node (Node(A true, B [1;1;1]), B [7;8;9])));;
let tree2 = Node(A false, B [1;2;3]);;
let tree3 = Node(Node(A true, Node (Node (A false, B [7;8;9]), B [4;5;6])), Node (A true, B [10;11;12]));;

let rec countA t =
        match t with
        | Node(A _, n) -> 1 + countA n
        | Node(n1, n2) -> countA n1 + countA n2
        | _ -> 0;;
// countA tree;;

// Soultion
let rec CountA t =
        match t with
        | Node(n1, n2) -> CountA n1 + CountA n2
        | A _ -> 1
        | B _ -> 0;;

let rec subst a a' b b' t =
    match t with
    | A x when x=a -> A a'  
    | B y when y=b -> B b'
    | A x  -> A x
    | B y -> B y
    | Node(n1, n2) -> Node(subst a a' b b' n1, subst a a' b b' n2);;
// subst true false [1;1;1] [0] tree;; 

// Solution
let rec Subst a a' b b' t =
    match t with
    | A x when x=a -> A a'  
    | B y when y=b -> B b'
    | Node(n1, n2) -> Node(Subst a a' b b' n1, Subst a a' b b' n2)
    | leaf -> leaf;;

let rec g = function
    | Node(t1,t2) -> Node(g t2, g t1)
    | leaf -> leaf;;
(* 
    Tree<'a, 'b> -> Tree<'a, 'b>
    Makes the mirroring of the a tree's branches.
*)

let rec f = function
    | A a -> ([a],[])
    | B b -> ([], [b])
    | Node(t1, t2) -> let (xs1, ys1) = f t1
                      let (xs2, ys2) = f t2
                      (xs1@xs2, ys1@ys2);;
(* 
    Tree<'a, 'b> -> 'a list * 'b list
    Creates a touple of lists containing the elements in the left sub-branches in the first list and the one in the right sub-branches
    in the second list.
*)

let rec F k = function
    | A a -> k([a], [])
    | B b -> k([], [b])
    | Node(t1, t2) -> F (fun (xs1, ys1) -> F (fun (xs2, ys2) -> k(xs1@xs2, ys1@ys2)) t2) t1;;
    

// PROBLEM 5
type T<'a> = N of 'a * T<'a> list;;

let td = N("g", []);;
let tc = N("c", [N("d",[]); N("e",[td])]);;
let tb = N("b", [N("c",[])]);;
let ta = N("a", [tb; tc; N("f",[])]);;

// Need a mutually recursive fun
let rec toList (N (v, ts)) = v::toListAux ts

and toListAux = function
    | [] -> []
    | n::ts -> toList n @ toListAux ts;;

// In one line can be done by:
let rec toListClever (N (v, ts)) = v::List.collect toListClever ts;;

let rec map f (N (v, ts)) = N(f v, mapRec f ts)
and mapRec f = function
    | [] -> []
    | t::ts -> map f t::mapRec f ts;;

// In one line can be done by:
let rec mapClever f (N (v, ts)) = N (f v, List.map (mapClever f) ts);;

type Path = int list;;

let rec isPath path t =
    match (path, t) with
    | ([], _) -> true
    | (i::path', N (v, ts)) when 0 <= i && i < List.length ts ->
        isPath path' (List.item i ts)
    | _ -> false;;

let rec get1 path (N (_, ts) as t) =
    match path with
    | [] -> t 
    | i::is -> get1 is (List.item i ts);;

let rec tryFindPathTo v (N (v', ts)) = 
    if v'=v then Some [] else tryFindInList 0 v ts 
and tryFindInList i v = function 
    | [] -> None 
    | N (v', _)::_ when v=v' -> Some [i] 
    | N (_, ts') :: ts-> 
        match tryFindInList 0 v ts' with 
        | None -> tryFindInList (i + 1) v ts 
        | Some is -> Some(i:: is);;