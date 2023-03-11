////////////////////////////////////// PROBLEM 1 //////////////////////////////////////

type Book = string
type Shelf = Book list // ordered alphabetically
type Date = int
type Name = string
type Loan = Book * Name * Date

let sh0 = ["Introduction to meta-mathematics"; "To mock a mockingbird"; "What is the name of this book"];;
let sh1 = ["a"; "a"; "c"; "c"; "e"; "f"; "g"; "g"];;
let ls0 = [("Communication and concurrency", "Bob", 4); ("Programming in Haskell", "Paul", 2); ("Communicating Sequential processes", "Mary", 7); ("Elements of the theory of computation", "Dick", 1)];;


let rec onShelf (b:Book) (bs:Shelf) = 
    match bs with
    | b'::rest when b' = b -> true
    | b'::rest -> onShelf b rest
    | [] -> false;;


let rec toShelf (b:Book) (bs:Shelf) = 
    match bs with 
    | [] -> [b]
    | b'::rest when b<=b' -> b::b'::rest
    | b'::rest  -> b'::toShelf b rest;;


let rec fromShelf (b:Book) (bs:Shelf) =
    match bs with
    | [] -> None
    | b'::rest when b=b' -> Some rest  
    | b'::rest -> match fromShelf b bs with
                  | None -> None
                  | Some(bs') ->  Some(b'::bs')


////////////////////////////////////// PROBLEM 2 //////////////////////////////////////

// Question 4
let f s xs = List.foldBack (fun x state -> (s, x)::state) xs [];;

////////////////////////////////////// PROBLEM 4 //////////////////////////////////////



// Not done