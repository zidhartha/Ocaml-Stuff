(*Lazy lists*)

type 'a llist = Cons of 'a * (unit -> 'a llist)


(*yvela naturaluri ricxvis listi*)
let rec lnat i = Cons (i, fun () -> lnat (i+1));;

let rec lfib = 
  let rec fib a b = 
    Cons (a,fun() -> fib b (a+b))
  in fib 0 1 

  let rec ltake  n (Cons(h,t)) =
    if n<0 then [] else h:: ltake (n-1) (t())   


    let rec lfilter f (Cons (h,t)) = 
      if f h then Cons (h,fun () -> lfilter f(t()))
      else lfilter f (t ())


    let z = ltake 12 (lnat 0) ;;



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
      List.filter (fun p -> p.psize = size) list
    
 
    let rec minhealth (list : peng list) (size : pengtype) : peng option =
      let type_list = pengs_with_type list size in
      match type_list with
      | [] -> None
      | h::t ->
        let min_penguin = List.fold_left (fun acc peng -> if peng.health < acc.health then peng else acc) h t in
        Some min_penguin

    


      let penguin_list = [
        { name = "Pingu"; health = 50; psize = Small };
        { name = "Skipper"; health = 80; psize = Average };
        { name = "Kowalski"; health = 65; psize = Average };
        { name = "Private"; health = 75; psize = Big };
        { name = "Rico"; health = 45; psize = Small }
      ];;

      let z = maxhealth penguin_list;;
      let x  = minhealth penguin_list ;;
