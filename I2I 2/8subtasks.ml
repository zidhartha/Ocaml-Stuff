(*Task 1*)

let rec member term list = 
  match list with 
  |[] -> false
  |h::t -> (h = term) || (member term t)


  let rec equals_second_components (z, x) y =
    match y with
    | (a, b) :: t -> (b = x) || equals_second_components (z, x) t
    | [] -> false
  


let z = equals_second_components (4,5) [(2,4);(5,5)];;


(*Task 2*)











(*Task 3*)


let rec drop_last list = 
  match list with
  |[] -> failwith "this list empty asf"
  |[h] -> []
  |h::t -> h::drop_last t 
  
  
  let z = drop_last [2;4;5;6;8];;



  (*Task 4*)
  let rec drop_Last_option list = 
    match list with
    |[] -> None
    |[h] -> Some []
    |h::t -> match drop_Last_option t with
             |None -> None
             |Some new_tail -> Some (h::new_tail)


  let x = drop_Last_option [2;4;5;6;7;8;9;135;436;25;325135;213];;


  (*Task 5*)


  let rec zip_With f list1 list2 = 
    match list1 with
    |[] -> []
    |h1::t1 -> begin 
       match list2 with
             |[] -> []
             |h2::t2 -> (f h1 h2) :: zip_With f t1 t2
    end

    let z =  zip_With (fun x y -> [x;y]) [1;2] [3;4;5];;