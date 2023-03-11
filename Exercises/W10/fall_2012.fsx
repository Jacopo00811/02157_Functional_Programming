// PROBLEM 1
type Name = string;;
type Score = int;;
type Result = Name * Score;;


let t1 = [("Me", 10); ("Mads", 20); ("Andrea", 31)];;
let t2 =  [("Me", 10); ("Mads", 20); ("Andrea", -30)];;


let legalResults rs = List.forall (fun (_, y) -> y>=0 && y<= 100) rs;; 


let rec maxL = function 
               | []       -> 0
               | [x]      -> x 
               | x::y::xs ->  maxL((max x y)::xs);; 

let maxScore rs = 
    let _, s = List.unzip rs
    maxL s;;


let best rs = List.find (fun (_, s) -> s=maxScore rs) rs;;


let average rs =
    let _, s = List.unzip rs
    (float (List.foldBack (+) s 0))/(float (List.length s));;


let rec delete r rs = 
    match rs with
    | e::rest when e=r -> rest
    | [] -> []
    | e::rest -> e::(delete r rest);;


let rec standing rs n c res=
    if c<=n then standing (delete (best rs) rs) n (c+1) (res@[best rs])
    else res;;


let bestN rs n =
    if n>List.length rs then failwith "Impossible to do this" 
    else standing rs n 1 [];;



// PROBLEM 2
type Typ = | Integer
           | Boolean 
           | Ft of Typ list * Typ;;

type Decl = string * Typ;;

type SymbolTable = Map<string,Typ>;;

type Exp = | V of string
           | A of string * Exp list;;

type Stm = 
    | Ass of string * Exp // assignment
    | Seq of Stm * Stm // sequential composition
    | Ite of Exp * Stm * Stm // if-then-else
    | While of Exp * Stm // while
    | Block of Decl list * Stm;; // block


let l1 = Map.ofList [("x", Integer); ("true", Boolean); ("4", Integer)];;
let l2 = [("x", Integer); ("true", Boolean); ("4", Integer); ("true", Boolean)];;
let l3 = [("x", Boolean); ("false", Boolean)];;
let st = Map.ofList [("x", Integer); ("y", Integer); (">", Ft([Integer; Integer], Boolean))];;
let e = A (">", [V "x"; V "y"]);;


let rec distinctVars dl = 
    match dl with
    | e::rest -> if (List.tryFind (fun x -> x=e) rest) = None then distinctVars rest else false
    | [] -> true;;


let toSymbolTable dl = Map.ofList dl;;


let rec extendST st dl = 
    match dl with
    | (k, v)::rest -> extendST (Map.add k v st) rest
    | [] -> st ;;


let rec symbolsDefined st e =
    match e with
    | V v -> Map.containsKey v st
    | A (s, vl) -> Map.containsKey s st && symbolsDefinedMutual st vl

and symbolsDefinedMutual st vl = List.forall (fun x -> symbolsDefined st x) vl;;

(*
let rec extract vl res = 
    match vl with
    | e::rest -> match e with
                 | V v -> extract rest (res@[v])
                 | _ -> extract rest res
    | [] -> res;;

let rec typOf st e = 
    match e with 
    | V v -> Map.find v st
    | A (s, vl) -> 
        let (l, r) = Map.find s st
        if List.forall (fun x -> List.find x (Findtype vl st)) l then r else failwith "Not possible"
         
and Findtype vl st = List.fold (fun state x -> (Map.find x st)::state) [] (extract vl [])


let rec wellTyped st stm = 
    match stm with
    | Ass (x, e) -> Map.containsKey x st && symbolsDefined st e && (Map.find x st = typOf st e)
    | Seq (stm1, stm2) -> wellTyped st stm1 && wellTyped st stm2
    | Ite (e, stm1, stm2) -> symbolsDefined st e && (typOf st e = Boolean) && wellTyped st stm1 && wellTyped st stm2
    | While (e, stm) -> symbolsDefined st e && (typOf st e = Boolean) && wellTyped st stm
    | Block (decls, stm) -> distinctVars decls && wellTyped (extendST st decls) stm
*)



// PROBLEM 3
let rec h a b =
    match a with
    | [] -> b
    | c::d -> c::(h d b);;
(*
    Type for h:
    'a list -> 'a list-> 'a list
    The function h appends the list b at the end of the list a. If the list a is empty it just returns the list b.
*)


type T<'a,'b> = | A of 'a | B of 'b | C of T<'a,'b> * T<'a,'b>;;

let T1 = C(A 3, B true);;


let T2 = C(A [], B None);;


let rec f1 = function
    | C(t1,t2) -> 1 + max (f1 t1) (f1 t2)
    | _ -> 1;;
(*
    Type for f1:
    T<'a,'b> -> int
    The function f1 counts the depth of a three and returns the maximum depth.
*)

let rec f2 = function
    | A e | B e -> [e]
    | C(t1,t2) -> f2 t1 @ f2 t2;;
(*
    Typer for f2:
    T<'a,'a> -> a' list 
    The function f2 gets all the leaf values in a tree and puts them in a list, traversing the three on the left branch first.
*)

let rec f3 e b t =
    match t with 
    | C(t1,t2) when b -> C(f3 e b t1, t2)
    | C(t1,t2)        -> C(t1, f3 e b t2)
    | _        when b -> C(A e, t)
    | _               -> C(t, B e);;
(*
    Type for f3:
    'a -> bool -> T<'a,'a> -> T<'a,'a>
    The function f3 scans a three and looks for the first leaf, setting b = "true" we will get the first left-leaf, setting b = "false"
    we will get the first right-leaf in a three.
*)
