(*Association lists *)

let rec is_empty list = match List.length list with
|0 -> true
|_ -> false 

let rec get k list = 
  match list with
  |[] -> false
  |(k',v):: t -> if k = k' then v else get k t 

  let rec put (k,v) list = match list with
  |[] -> [(k,v)]
  |(k',v')::t -> if k = k' then ((k,v') :: t) else ((k',v') :: (put (k,v) list)) 


  let rec containts_key k list = 
    match list with
    |[] -> false
    |(k',v) :: t -> (k=k') || containts_key k t 

  
let rec remove k list = 
  match list with
  |[] -> []
  |(k',v) :: t -> if (k = k') then t else (k',v) :: (remove k t) 


  let rec keys list = 
    match list with
    |[] -> []
    |(k,v)::t -> [k] @ keys t
    
    
    let rec values list = 
      match list with
      |[] -> []
      |(_,v) :: t -> v :: values list

    let z = values [(2,"das");(4,"dawf");(42,"Sdawd")];;
