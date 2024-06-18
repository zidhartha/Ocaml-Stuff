type pengtype = Small|Average|Big ;;
    type peng = {name : string; health : int ;psize:pengtype};;

    let rec maxhealth (list : peng list) : peng option = 
    match list with
    |[] -> None
    |h::t -> begin
      match maxhealth t with
      |None -> Some h
      |Some peng -> if h.health > peng.health then (Some h) else (Some peng)
    end


    let pengs_with_type list size = 
      List.filter (fun p -> p.psize = size ) list
  
      let rec minhealth (list: peng list) (size: pengtype) : peng option = 
        let filtered_list = pengs_with_type list size in
          match filtered_list with
          |[] -> None
          |h::t -> 
            let min_penguin = List.fold_left (fun acc x -> if x.health > acc.health then acc else x ) h t in 
          Some min_penguin 



      let penguin_list = [
        { name = "Pingu"; health = 50; psize = Small };
        { name = "Skipper"; health = 80; psize = Average };
        { name = "Kowalski"; health = 65; psize = Average };
        { name = "Private"; health = 75; psize = Big };
        { name = "Rico"; health = 45; psize = Small }
      ];;

      let z = maxhealth penguin_list;;
      let x  = minhealth penguin_list Average ;;
