function
| 2 -> 28 // February
| 4|6|9|11 -> 30 // April, June, September, November with the OR-pattern
| _ -> 31 // All other months
;;

(* Declering the name of the function *)
let daysOfMonth = function
| 2 -> 28 // February
| 4|6|9|11 -> 30 // April, June, September, November
| _ -> 31 // All other months