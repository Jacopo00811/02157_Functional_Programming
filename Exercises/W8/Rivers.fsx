type Name = string
type Flow = int // can be assumed positive in below questions
type River = R of Name * Flow * Tributaries
and Tributaries = River list

let riv = R("R", 10, [R("R1", 5, []); R("R2", 15, [R("R4", 2, [])]); R("R3", 8, [])])
let riv3 = R("R3", 8, []) 

let contains n r =
    match r with
    | R(n', _, t) -> if n'=n then true 
                        else let rec containsRec n t = 
                                match t with
                                | R(n'', _, _)::_ when n''=n -> true
                                | R(n'', _, [])::rest -> containsRec n rest
                                | R(n'', _, t')::rest -> containsRec n rest || containsRec n t'
                                | [] -> false
                             containsRec n t;;             


let rec allNamesRec n t = 
    match t with
    | R(n', _, [])::rest -> n'::allNamesRec n rest
    | R(n', _, t')::rest -> n'::(allNamesRec n t')@(allNamesRec n rest)
    | [] -> [n] // adds a duplicate at the end

let allNames r =
    match r with
    | R(n, _, []) -> [n]
    | R(n, _, t) -> match List.rev (allNamesRec n t) with
                    | _::xs -> xs
                    | [] -> [];;    


let rec totalFlowRec f t =
    match t with 
    | R(_, f', [])::rest -> f'+(totalFlowRec f rest)
    | R(_, f', t')::rest -> f'+(totalFlowRec f t')+(totalFlowRec f rest)
    | [] -> f

let totalFlow r = 
    match r with 
    | R(_, f, []) -> f
    | R(_, f, t) -> (totalFlowRec f t)-f // Because otherwise it is added two times. 

// Complicated solutions but at least it works.
let rec mainSourceRec n f t = 
    match t with 
    | R(n', f', [])::rest when f'>f -> mainSourceRec n' f' rest
    | R(n', f', t')::rest when f'>f -> if mainSourceRec n' f' t' > mainSourceRec n' f' rest then mainSourceRec n' f' t' else mainSourceRec n' f' rest
    | R(n', f', [])::rest -> mainSourceRec n f rest
    | R(n', f', t')::rest -> if mainSourceRec n f t' > mainSourceRec n f rest then mainSourceRec n f t' else  mainSourceRec n f rest  
    | [] -> (n, f)

let mainSource r =
    match r with
    | R(n, f, []) -> (n, f)
    | R(n, f, t) -> mainSourceRec n f t

// Doesn't work but the idea is this one
let rec tryInsertRec n t r = 
    match r with
    | R(n', f, t')::rest when n=n' -> Some(R(n, f, t::t'))
    | R(n, _, [])::rest -> tryInsertRec n t rest
    | R(n', f, t')::rest -> if contains n (R(n', f, t')) then tryInsertRec n t t' else tryInsertRec n t rest
    | [] -> None // I don't think will ever be matched but okay

let tryInsert n t r = if not(contains n r) then None 
                        else match r with
                             | R(n, f, []) -> Some(R(n, f, [t]))
                             | R(n', f, t') when n=n' -> Some(R(n, f, t::t'))
                             | R(_, _, t') -> tryInsertRec n t t'


// The problem is that you could end up with infinite recursion if a river is not well defined.
// Solution 
(*
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

*)