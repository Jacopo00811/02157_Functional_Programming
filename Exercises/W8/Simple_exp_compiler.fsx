type exp = 
    | C of int
    | BinOp of exp * string * exp
    | Id of string
    | Def of string * exp * exp;;

let exp1 = BinOp(C(3), "+", BinOp(C(2), "*", C(2)))
let exp2 = BinOp(C(0), "+", C(0))
let exp3 = BinOp(C(1), "*", C(25)) 


let rec toString exp =
    match exp with
    | C(i)            -> sprintf "%i" i // format number as string
    | BinOp(e1, "+", e2) -> sprintf "(%s + %s)" (toString e1) (toString e2)
    | BinOp(e1, "*", e2) -> sprintf "(%s * %s)" (toString e1) (toString e2)
    | BinOp(e1, "-", e2)-> sprintf "(%s - %s)" (toString e1) (toString e2)
    | BinOp(e1, _ , e2) -> failwith "This operator is not supported"
    | Id(_) -> "var"
    | Def(_, _, _) -> "def"


let rec extractOp exp = 
    match exp with
    | C(i) -> Set.ofList []
    | BinOp(e1, op, e2) -> Set.add(op) (Set.union (extractOp e1) (extractOp e2))
    | Id(_) -> Set.ofList []
    | Def(_, _, _) -> Set.ofList []

// For lists would have been op::(extractOp e1)@(extractOp e2)

let def1 = Def("x", C 5 , BinOp(Id "x", "+", Id "x"))
let def2 =Def("y", C 8, Def("x", C 5 , BinOp(Id "x", "+", Id "t")))

let rec extractDef exp =
    match exp with 
    | C(i) -> Set.ofList []
    | Def(var, e1, e2) -> Set.add(var) (Set.union (extractDef e1) (extractDef e2))
    | BinOp(_, _, _) -> Set.ofList []
    | Id(_) -> Set.ofList []

let rec extractId exp = 
    match exp with 
    | C(_) -> Set.ofList []
    | Def(_, e1, e2) -> Set.union (extractId e1) (extractId e2)
    | BinOp(Id(var1), _, Id(var2)) -> Set.ofList(var1::[var2])
    | BinOp(e1, _, e2) -> Set.union (extractId e1) (extractId e2)
    | Id(_) -> Set.ofList []

let isDef exp = 
    if extractDef exp = extractId exp then true else false


