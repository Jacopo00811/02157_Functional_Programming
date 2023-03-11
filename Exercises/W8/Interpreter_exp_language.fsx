type ExprTree = | Const of int
                | Ident of string
                | Minus of ExprTree
                | Sum of ExprTree * ExprTree
                | Diff of ExprTree * ExprTree
                | Prod of ExprTree * ExprTree
                | Let of string * ExprTree * ExprTree;;


let et =
    Prod(Ident "a",
        Sum(Minus (Const 3),
            Let("x", Const 5, Sum(Ident "x", Ident "a"))));;

let env = Map.add "a" -7 Map.empty;;


let rec eval t env =
    match t with
    | Const n -> n
    | Ident s -> Map.find s env
    | Minus t -> - (eval t env)
    | Sum(t1,t2) -> eval t1 env + eval t2 env
    | Diff(t1,t2) -> eval t1 env - eval t2 env
    | Prod(t1,t2) -> eval t1 env * eval t2 env
    | Let(s,t1,t2) -> let v1 = eval t1 env
                      let env1 = Map.add s v1 env
                      eval t2 env1;;