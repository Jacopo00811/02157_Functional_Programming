// PROBLEM 3
// 1
let maxL l = 
    let rec maxLRec (l, m) =
        match l with
        | [] -> m
        | x::xs -> maxLRec (xs, if x > m then x else m)
    maxLRec (l, 0)

// 2
type Title = string;;
type Section = Title * Elem list
and Elem = Par of string | Sub of Section;;
type Chapter = Title * Section list;;
type Book = Chapter list;;

let sec11 = ("Background", [Par "bla"; Sub(("Why programming", [Par "Bla."]))]);;
let sec12 = ("An example", [Par "bla"; Sub(("Special features", [Par "Bla."]))]);;
let sec21 = ("Fundamental concepts", [Par "bla"; Sub(("Mathematical background", [Par "Bla."]))]);;
let sec22 = ("Operational semantics", [Sub(("Basics", [Par "Bla."])); Sub(("Applications", [Sub (("Details", [Par "Bla."]))]))]);;
let sec23 = ("Further reading", [Par "bla"]);;
let sec31 = ("Overview", [Par "bla"]);;
let sec32 = ("A simple example", [Par "bla"]);;
let sec33 = ("An advanced example", [Par "bla"]);;
let sec41 = ("Status", [Par "bla"]);;
let sec42 = ("What's next?", [Par "bla"]);;
let ch1 = ("Introduction", [sec11;sec12]);;
let ch2 = ("Basic Issues", [sec21;sec22;sec23]);;
let ch3 = ("Advanced Issues", [sec31;sec32;sec33]);; //sec34 is not defined
let ch4 = ("Conclusion", [sec41;sec42]);;
let book1 = [ch1; ch2; ch3; ch4];;

let rec overviewRec k b =
    match b with
    | c::ctail -> let (t,_) = c 
                  overviewRec (fun v -> k(t::v)) ctail
    | [] -> k [];;


let overview b = overviewRec id b;;

// 3
let rec counter_sec (_,es) = 1 + List.fold (fun state i -> 
                                match i with
                                | Par _ -> 0
                                | Sub s' -> max (counter_sec s') state) 0 es;;

let depthSection s = 1+(counter_sec s);;


let rec counter_elem (_,es) = 1 + List.fold (fun state i -> 
                                match i with
                                | Par _ -> 1
                                | Sub s' -> max (counter_sec s') state) 0 es;;

let depthElem s = 1+(counter_elem s);;

(*
let rec depthSection(_,es) = 1 + maxL(List.map depthElem es)

and depthElem = function | Par _ -> 0
                         | Sub s -> depthSection s;;

let depthChapter(_,ss) =  1 + maxL(List.map depthSection ss)  

let depthBook(cs) = maxL(List.map depthChapter cs);;

*)

// 4
type Numbering = int list;;
type Entry = Numbering * Title;;
type Toc = Entry list;;

let rec tocB(cs) = tocChapters cs 1 

and tocChapters cl n   = match cl with 
                         | [] -> [] 
                         | (t,ss)::cs ->  ([n],t)::tocSections ss [n] 1 @ tocChapters cs (n+1)

and tocSections secs ns i = match secs with 
                            | []    -> []
                            | s::ss -> tocSection s ns i @ tocSections ss ns (i+1)

and tocSection(t,es) ns i = let ns'=ns@[i]
                            (ns',t) :: tocElems es ns' 1

and tocElems es ns i = match es with 
                       | []         -> []
                       | Par _::es  -> tocElems es ns i
                       | Sub s:: es -> tocSection s ns i @ tocElems es ns (i+1);;


let toc1 = tocB book1;;  


// PROBLEM 1
// 1
(*
f [1;6;0;8] [0;7;3;3] 
-> 1::0::f [6;0;8] [7;3;3]
-> 1::0::6::7::f [0;8] [3;3]
-> 1::0::6::7::0::3::f [8] [3]
-> 1::0::6::7::0::3::8::3::f [] []
-> 1::0::6::7::0::3::8::3::[]::[]
-> [1;0;6;7;0;3;8;3]
*)

// 2
(*
    'a list -> 'a list -> 'a list  
    The function mixes up two list combinig the elements of the first ones with the ones in the second one.
*)

// 3 
(*
    The function is not tail recursive because the last call is the the recusion step.
*)
let rec f (xs, ys, res) =
    match (xs, ys, res) with
    | (x::xtail, y::ytail, res) -> f (xtail, ytail, y::x::res)
    | _ -> List.rev res

// 4 
let rec F k xs ys =
    match (xs, ys) with
    | (x::xtail, y::ytail) -> F (fun res -> k(x::y::res)) xtail ytail
    | _ -> k [];;



// PROBLEM 2.1
// 1
let rec FF = function
    | 0 -> [0]
    | i when i>0 -> i::g(i-1)
    | _ -> failwith "Negative argument"
and g = function
    | 0 -> []
    | n -> FF(n-1);;
(*
    FF 5
    int -> int list
    int -> int list
    The function, given an integer creates a list of decrasing odd/even numbers until 0 or 1, dependig on the given integer
*)
let h s k = seq { for a in s do
                    yield k a };;
(*
    h (seq [1;2;3;4]) (fun i -> i+10)
    seq<int> -> (int -> int) -> seq<int>
    seq<'a> -> ('a -> 'b) -> seq<'b>
    The function gets a sequence and another function and applies it the the given sequence creating a new one.
*)


// PROBLEM 3 
type Container =
    | Tank of float * float * float // (length, width, height)
    | Ball of float // radius

// 1
let tank = Tank (9.5, 6., 8.);;
let ball = Ball 3.14;;

// 2
let isWF c =
    match c with
    | Tank (l, w, h) -> if l>0 && w>0 && h>0 then true else false
    | Ball r -> if r>0 then true else false;;

// 3
let volume c = 
    match c with
    | Tank (l, w, h) -> l*w*h
    | Ball r -> 4./3.*System.Math.PI*r**3;;

// 4
type Containers =
    | Tank of float * float * float // (length, width, height)
    | Ball of float // radius
    | Cylinder of float * float // (radius, height)

let IsWF c =
    match c with
    | Tank (l, w, h) -> if l>0 && w>0 && h>0 then true else false
    | Ball r -> if r>0 then true else false
    | Cylinder (r, h) -> if r>0 && h>0 then true else false;;

let Volume c = 
    match c with
    | Tank (l, w, h) -> l*w*h
    | Ball r -> 4./3.*System.Math.PI*r**3
    | Cylinder (r, h) -> System.Math.PI*r**2*h;;

type Name = string
type Contents = string
type Storage = Map<Name, Contents*Containers>

// 5
let stg = Map.ofList [("tank1", ("oil", Tank (9.5, 6., 8.))); ("ball1", ("water", Ball 3.14 ))];;

// 6
let find n stg =
    match Map.tryFind n stg with
    | Some (cnt, c) -> (cnt, Volume c)
    | None -> failwith "This container is not in the storage";;


// PROBLEM 4 
// 1
type T<'a> = L | N of T<'a> * 'a * T<'a>

let t = N(N(L, 1, N(N(L, 2, L), 1, L)), 3, L);;
(*
    T<int>
*)
let t1 = N(N(L, [false; true; false], N(N(L, [true; false], L), [true], L)), [true; false; true], L);;
let t2 = N(L, [true], L);;
let t3 = N(L, [false], N(L, [true; false], L));;

// 2
(* let rec f g t1 t2 =
    match (t1,t2) with
    | (L,L) -> L
    | (N(ta1,va,ta2), N(tb1,vb,tb2))
        -> N(f g ta1 tb1, g(va,vb), f g ta2 tb2);;
*)
(*
    f has type: ('a*'b -> 'c) -> T<'a> -> T<'b> -> T<'c>
    f applies a function g to the tuple of middle-values obtained form two trees that are passed as input.
*)

(*let rec h t = 
    match t with
    | L -> L
    | N(t1, v, t2) -> N(h t2, v, h t1);;
 *)   
(*
    h has type: T<'a> -> T<'a>
    h does the mirroring of a tree switching the left and right sub-branches
*)

(*
let rec g = function
    | (_,L) -> None
    | (p, N(t1,a,t2)) when p a -> Some(t1,t2)
    | (p, N(t1,a,t2)) -> match g(p,t1) with
                         | None -> g(p,t2)
                         | res -> res;;
*)
(*
    g has type: ('a -> bool)*T<'a> -> (T<'a>*T<'a>) option
    g check if the boolean expression it true for the middle element of a tree. If it is true then the function returns an option 
    with a tuple of the two sub-trees, otherwise goes on with the left sub-tree fisrt trying to find a middle-value that evaluates to 
    true (recalling the function g).
*)

// 3
let rec count a t =
    match t with
    | L -> 0
    | N(t1,a',t2) when a=a' -> 1+count a t1+count a t2
    | N(t1,_,t2) -> count a t1+count a t2;;

// 4
let rec replace a b t =
    match t with
    | L -> L
    | N(t1,a',t2) when a=a' -> N(replace a b t1, b, replace a b t2)
    | N(t1,a',t2) -> N(replace a b t1, a', replace a b t2);;