(* I would use sets so you are sure that there is no repetiotions is the register*)

type Name = string 
type PhoneNumber = string // could also be integers
type YearOfBirth = int
type Interest = string
type ThemesOfInterest = Interest list
type EmployeeDescription = PhoneNumber*YearOfBirth*ThemesOfInterest
type Employee = Name*EmployeeDescription
type Register = Employee list

type ClubArrangement = EmployeeDescription -> bool

let alice: Employee = ("Alice", ("123", 1988, ["soccer"; "jazz"]))
let bob: Employee = ("Bob", ("456", 1967, ["soccer"; "fishing"]))
let charlie: Employee = ("Charlie", ("789", 1991, ["fishing"; "jazz"]))
let register: Register = [alice; bob; charlie]

// val isMember: 'a -> 'a list -> bool when 'a: equality
let rec isMember x = function
    | [] -> false 
    | x'::xs -> x=x' || isMember x xs;;

// val p1: ClubArrangement
let p1 (_, yb, ths) = yb>1882 && isMember "soccer" ths && isMember "jazz" ths
// val p2: ClubArrangement
let p2 (_, yb, ths) = yb>1882 && (isMember "soccer" ths || isMember "jazz" ths)


// val extractInterested: ClubArrangement -> Register -> (Name*PhoneNumber) list
let rec extractInterested p = function
    | [] -> []
    | (n, (no, yb, ths))::emps ->
        if p (no, yb, ths)
        then (n, no)::extractInterested p emps
        else extractInterested p emps 

// Test
let test1 = extractInterested p1 register = [("Alice", "123")];;
let test2 = extractInterested p2 register = [("Alice", "123"); ("Charlie", "789")];;
let test3 = extractInterested p1 [] = [];;