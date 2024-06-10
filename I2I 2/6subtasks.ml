(*First subtask*)

let f1 lst = List.fold_left (fun acc (a,b) -> (b,a) :: acc) [];;

f1 [(1,2);(3,4)];;

let f2 lst = List.fold_left(fun acc x -> if(List.length acc) mod 2 = 0 then acc @ [x] else x::acc) [] ;; 
  

f2 [] [2;4;5;6;7;8;9;18;11;2;31];;



let f3 lst =  lst @ lst;;

(*Second subtask*)



(*Third subtask*)
type 'a llist = Cons of 'a * (unit -> 'a llist)


(*yvela naturaluri ricxvis listi*)
let rec lnat i = Cons(i, (fun () -> lnat (i+1)))

let rec map (Cons (f a ,fun () -> map f(b) ()));;












(*Fourth subtask*)









(*Fifth subtask*)


let rec nodup (Cons(a,b)) = 
  match b () with 
  Cons(x,_) -> if x = a then nodup(b()) else Cons(a,fun() -> nodup (b()));;






  (*Sixth subtask*)

  let rec hamming = 
    let rec impl a = if (a mod 2 = 0) || (a mod 3 = 0) || (a mod 5 = 0) 

