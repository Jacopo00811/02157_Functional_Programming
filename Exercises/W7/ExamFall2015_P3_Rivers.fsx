// Michael R. Hansen 09-11 2021

// Fall Exam 2015, Problem 3 35 %

type Name       = string
type Flow       = int
type River      = R of Name * Flow * Tributaries
and Tributaries = River list


// 1
let r1 = R("R1", 5, [])
let r3 = R("R3", 8, [])
let r4 = R("R4", 2, [])
let r2 = R("R2", 15, [r4])
let riv = R("R", 5, [r1;r2;r3]);;

// 2
let rec contains n = 
   function
   | R(n',_,_) when n=n' -> true
   | R(_,_,ts)           -> List.exists (contains n) ts;;

// alternativ løsning
let rec contains1 n = 
   function
   | R(n',_,_) when n=n' -> true
   | R(_,_,ts)           -> containsTs n ts
and containsTs n = 
   function
   | []     -> false
   | r::ts -> contains1 n r || containsTs n ts;;

// 3                          
let rec allNames (R(n,fl,ts)) = n::List.collect allNames ts     

// 4
let rec totalFlow (R(_,f,ts)) = List.fold (fun s r -> s + totalFlow r) f ts;;

// alternative
let rec totalFlow1 (R(_,f,ts)) = f + totalFlowTs ts
and totalFlowTs = function
             | []    -> 0
             | r::ts -> totalFlow1 r + totalFlowTs ts;;

// 5
let maxFlow (n,fl) (n',fl') = if fl>fl' then (n,fl) else (n',fl') 
                                    
let rec mainSource (R(n,fl,ts)) = mainSourceList (n,fl) ts
and mainSourceList res = 
   function
   | []    -> res
   | r::ts -> mainSourceList (maxFlow res (mainSource r)) ts       


// 6
let rec tryInsertTributary n t = 
   function
   | R(n',fl,ts) when n=n' -> Some(R(n',fl,t::ts))
   | R(n',fl,ts)           -> match tryInsertTributaryInList n t ts with
                                  | None     -> None
                                  | Some ts' -> Some(R(n',fl,ts'))
and tryInsertTributaryInList n t =
   function 
   | []    -> None
   | r::ts -> match tryInsertTributary n t r with 
              | None    -> match tryInsertTributaryInList n t ts with
                           | None     -> None
                           | Some ts' -> Some(r::ts')
              | Some r' -> Some(r'::ts);;
              
// 7 
// A river may split (river bifurcaion), that is, it divides into two streams. 
// The streams (from a river that splits), may actually merge again.
// The tree model does not capture these cases.  
// A model based on directed acyclic graphs would be adequate in these cases.


// How can the low-level exception handling in tryInsert.... be hidden?

// How can tedious Some/None business be hidden?

// Use monadic-based programming

let ret n = Some n

let fail() = None

let bind (comp,f) = match comp with
                    | None   -> None 
                    | Some v -> f v;;

let (>>=) comp f = bind(comp,f);;

let either comp g f = match comp with
                      | None   -> f
                      | Some v -> g v;;  

let rec insertTributary n t = 
   function
   | R(n',fl,ts) when n=n' -> ret(R(n',fl,t::ts))
   | R(n',fl,ts)           -> insertTributaryInList n t ts >>= fun ts' -> ret (R(n',fl,ts'))
and insertTributaryInList n t =
   function 
   | []    -> fail()
   | r::ts -> either 
                (insertTributary n t r)    
                (fun r' -> ret(r'::ts))
                (insertTributaryInList n t ts >>= fun ts' -> ret(r::ts'));;

insertTributary "R4" (R("R5", 11, [])) riv;;