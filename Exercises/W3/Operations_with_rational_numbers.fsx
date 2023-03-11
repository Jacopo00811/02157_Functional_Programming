type Qnum = int*int;; // (a,b) where b>0 ans gcd(a,b)=1


let rec gcd = function
| (0,n) -> n
| (m,n: int) -> gcd(n % m,m);;


let canc(p,q) =
    let sign = if p*q < 0 then -1 else 1
    let ap = abs p
    let aq = abs q
    let d = gcd(ap,aq)
    (sign * (ap / d), aq / d);;


let mkQ = function
    | (_,0) -> failwith "Division by zero"
    | pr -> canc pr;;


let (.+.) (a,b) (c,d) = canc(a*d + b*c, b*d);; // Addition
let (.-.) (a,b) (c,d) = canc(a*d - b*c, b*d);; // Subtraction
let (.*.) (a,b) (c,d) = canc(a*c, b*d);; // Multiplication
let (./.) (a,b) (c,d) = (a,b) .*. mkQ(d,c);; // Division
let (.=.) (a,b) (c,d) = (a,b) = (c,d);; // Equality


let toString(p:int,q:int) = (string p) + "/" + (string q);; // Convert the fraction to a string
