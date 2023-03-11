// Michael R. Hansen  27-10-2022
module Luggage

type Lid     = string 
type Flight  = string 
type Airport = string

type Route    = (Flight * Airport) list

type  LuggageCatalogue = (Lid * Route) list 


let rec inRoute f = function
                    | []           -> false               // c1
                    | (f1,_)::rest -> if f=f1             // c2
                                      then true     
                                      else inRoute f rest 

(* An erronuous and weird "solution":             *)

let rec withFlight f = 
   function 
   | [(lid, (f1,_)::_)] when f=f1     -> [lid]
   | [(lid, _::rest)]                 -> withFlight f [(lid,rest)] 
   | (lid, (f1,_)::_)::rest when f=f1 -> lid::withFlight f rest
   | (_,[])::rest                     -> withFlight f rest
   | _                                -> [""]

(* A well-designed implementation 
let rec withFlight f = 
   function
   | []                           -> []
   | (lid,r)::lc when inRoute f r -> lid ::withFlight f lc
   | _::lc                        -> withFlight f lc

*)
