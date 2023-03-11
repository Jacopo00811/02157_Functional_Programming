(*


type AExp = (* Arithmetical expressions *)
    | N of int (* numbers *)
    | V of string (* variables *)
    | Add of AExp * AExp (* addition *)
    | Mul of AExp * AExp (* multiplication *)
    | Sub of AExp * AExp;; (* subtraction *)

type State = Map<string,int>;;
(*
type BExp = (* Boolean expressions *)
    | TT (* true *)
    | FF (* false *)
    | Eq of //.... (* equality *)
    | Lt of //.... (* less than *)
    | Neg of //.... (* negation *)
    | Con of //.... ;; (* conjunction *)
*)

type Stm = (* statements *)
    | Ass of string * AExp (* assignment *)
    | Skip
    | Seq of Stm * Stm (* sequential composition *)
    | ITE of BExp * Stm * Stm (* if-then-else *)
    | While of BExp * Stm;; (* while *)


let rec A a s =
    match a with
    | N n -> n
    | V x -> Map.find x s
    | Add(a1, a2) -> A a1 s + A a2 s
    | Mul(a1, a2) -> A a1 s * A a2 s
    | Sub(a1, a2) -> A a1 s - A a2 s;;

(*
let rec B b s =
    match b with
    | TT -> true
    | ....
*)

let update x v s = Map.add x v s;;

(*
let rec I stm s =
    match stm with
    | Ass(x,a) -> update x ( ... ) s
    | Skip -> ...
    | Seq(stm1, stm2) -> ...
    | ITE(b,stm1,stm2) -> ...
    | While(b, stm) -> ... ;;
*)





// Example factorial

(*
y:=1 ; while !(x=0) do (y:= y*x;x:=x-1)
*)

let fac = Seq(Ass("y", N 1),
    While(Neg(Eq(V "x", N 0)),
        Seq(Ass("y", Mul(V "x", V "y")) ,
            Ass("x", Sub(V "x", N 1)) )));;

(* Define an initial state *)
let s0 = Map.ofList [("x",4)];;

(* Interpret the program *)
let s1 = I fac s0;;


*)