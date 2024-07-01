(*Basic example of a map in ocaml.Basically a HashMap from java*)

type day = Sun |Mon |Tue |Wed |Thu | Fri | Sat


let int_of_day = function
    |Mon -> 1
    |Tue -> 2
    |Wed -> 3
    |Thu -> 4
    |Fri -> 5
    |Sat -> 6
    |Sun -> 7


module DayKey = struct 
 type t = day 
 let compare t1 t2 = 
 int_of_day t1 - int_of_day t2
end


module DayMap = Map.Make(DayKey)
let m = 
  let open DayMap in 
 empty  
 |> add Mon "Monday"
 |> add Tue "Tuesday"