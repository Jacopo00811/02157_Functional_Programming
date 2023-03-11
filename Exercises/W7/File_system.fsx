type FileSys = Element list
and Element =
    | File of string
    | Dir of string * FileSys;;


let d1 = Dir("d1",[File "a1"; Dir("d2", [File "a2"; Dir("d3", [File "a3"])]); File "a4"; Dir("d3", [File "a5"])])


// Mutually recursive functions
let rec namesFileSys = function
    | [] -> []
    | e::es -> (namesElement e) @ (namesFileSys es)
and namesElement = function
    | File s -> [s]
    | Dir(s,fs) -> s :: (namesFileSys fs) ;;