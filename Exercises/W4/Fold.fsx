List.fold (-) 0 [1; 2; 3];;


// Using cons in connection with fold gives the reverse function:
let rev xs = List.fold (fun rs x -> x::rs) [] xs;;
// But exists also List.rev