let f1 acc v = acc + 1;;

List.fold_left f1 0 [2;4;5;6;7];;

let f2 acc v = if List.length v > List.length acc then v else acc

let f3 (a,b) acc = [(b,a)] @ acc;;

let f4 acc v = v::List.rev acc;;

let f5 acc (k,v) = fun x -> if x = k then v else acc x


let maximum list = 
  match list with
  |[] -> failwith ""
  |h::t -> List.fold_left (fun acc h -> if acc>=h then acc else h ) h t


  let z = maximum [2;4;5;6;7;8;6;2;410];;


  let reverse list = 
    match list with
    |[] -> []
    |h::t -> List.fold_left (fun acc h -> h::acc) [] t
    let z = reverse [2;4;5;6;7;8;6;2;410];;


    let map_over_list f list = List.fold_left (fun acc x -> (f x) :: acc) list []
