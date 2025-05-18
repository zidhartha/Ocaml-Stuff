
(*product*)

let rec product_of_list list = 
  match list with
  |[] -> 1
  |h::t -> h * (product_of_list t) 

  let () = assert (product_of_list @@ 2::3::5::[] = 30)


  let rec concat list = 
    match list with
    |[] -> ""
    |h::t -> h ^ (concat t) 
    
    let _ = assert(concat @@ "Java" :: "is" :: "better" ::[] = "Javaisbetter");;


    (*patterns*)


    let has_bingred list = 
     match list with
     |[] -> false
     |h::t -> if h = "bingred" then true else false     

     let _ = assert(has_bingred @@ "bingred" :: [])
     let _ = assert(has_bingred @@ "bingre" :: [])


     let has_2_or4_elements list = 
      match list with
      |_::_::[] -> true
      |_::_::_::_::[] -> true
      |_ -> false 

      let _ = assert(has_2_or4_elements @@ 2::4::[])


      let first_Two list = 
        match list with 
        |[] -> failwith "empty list" 
        |h::t -> match t with
                 |[] -> failwith "only one element in the list" 
                 |[x] -> x = h 

let _ = assert(first_Two @@ 2::2::[])


(*library*)


let fifth_element list =
     if(List.length list < 5) then
      failwith "list does not have the fifth element"
    else 
      List.nth list 5 


      let sort_Reverse list = 
        List.rev @@ (List.sort Stdlib.compare list)

(*library puzzle*)

let get_last lst = List.nth lst @@ List.length lst - 1;; 


let any_zeroes list = List.exists (fun x -> x=0) list ;;

let rec any_zeroes1 list = 
  match list with
  |[] -> false
  |h::t -> if h = 0 then true else any_zeroes1 t
  (*chemi versia amis ;* *)



(*take drop*)
let rec take n list = 
  match n with
  |0 -> []
  |x -> match list with
        |[] -> list
        |h::t -> h :: take (n-1) t 

let z = take 5 [2;4;5;6;7;8;9;0;1;12;3];;




let rec drop n list  =
match n with
|0 -> list
|r -> match list with
      |[] -> []
      |h::t -> drop (r-1) t



  (*Unimodal*)
  

  (*es funqcia mxolod mashin iwyeba rodesac qvemota funqcia dainaxavs rom romelime nawilshi listis elementebi shemcirebas iwyeben*)
  let rec is_mon_dec list = 
    match list with
    |[] | [_] -> true
    |h :: (h1::t1 as t) -> h >=h1 && is_mon_dec t 

  let rec is_mon_inc_then_dec list = 
    match list with
    |[] | [_] -> true
    |h :: (h1::t1 as t) as lst -> if h <=h1 then is_mon_inc_then_dec t else is_mon_dec lst 

  let is_unimodal list = 
    is_mon_inc_then_dec list 


    let x = [2;3;4;3;2];;
    let z = is_unimodal x;;


    (*Power set*)

    let rec powerset = function
  | [] -> [ [] ]
  | x :: s -> let p = powerset s in
    List.map (List.cons x) p @ p


    (*print int list rec*)

  let rec print_int_list_rec list = 
    match list with 
    |[] -> ()
    |h :: t -> print_endline @@ string_of_int h; print_int_list_rec t 


    


    (*print in list iter*)

    let print_int_list lst =
      List.iter (fun x -> print_endline @@ string_of_int x) lst

      let z = print_int_list [2;4;5;6;7;8];;



      (*student*)
  type student = {first_name : string; last_name : string; gpa : float};;

  let dato = {first_name = "dato"; last_name = "jincharadze" ; gpa = 3.0};;

 let get_name student = 
  student.first_name ^ " " ^ student.last_name
 
  let create_student name surname gpa = {first_name = name; last_name = surname;gpa = gpa};;
    
 
  (*pokecord*)

  type poketype = Normal|Fire|Water ;;

  type pokemon = {name : string;hp : int; ptype : poketype};;

  let charizard = {name ="charizard" ; hp = 78 ;ptype = Fire};;

  let squirtle = {name = "squirtle" ; hp = 44;ptype = Water};;



  (*safe hd and tail*)

  let safe_hd list = 
    match list with
    |[] -> None
    |h::t -> Some h 


    let safe_tl list = 
      match list with
      |[] -> None
      |_::t -> Some t 


    (*pokefun*)

    let rec max_hp (list : pokemon list) : pokemon option = 
      match list with
      |[] -> None
      |h::t -> begin
        match max_hp t with
        |None -> Some h 
        |Some pokemon -> Some(if h.hp >= pokemon.hp then h else pokemon)
      end
     (*es funqcia rekursiulad chadis listis bolomde da mandedan iwyebs ori elementis hpebis shedarebas.*)
      let pokeList = [charizard;squirtle];;
      let z = max_hp pokeList;;

      (*date before*)

      type date = int * int * int ;;
      
      let is_before date1 date2 = 
        let (y1,m1,d1) = date1 in
        let (y2,m2,d2) = date2 in
        (y1<y2)||
        (m1<m2 && y1=y2) ||
        (m1=m2 && y1=y2 && d1<d2)


        (*earliest date*)

    let rec earliest list = 
      match list with
       |[] -> None
      |h::t -> begin
       match earliest t with
        |None -> Some h
        |Some date1 -> Some (if is_before h date1 then h else date1) 
      end

          (*es funqcia igebs datebis lists da cdilobs is_before funqciis daxmarebit naxos yvelaze adre romeli date moxdeba.
             am funqciis meore nawili rekursiulad chadis listis bolo elementamde da mandedan iwyebs ukan amosvlas da amavdroulad
             bolo or elements adarebs ertmanets da am oridan yvelaze adre myops tovebs.radganac es funqcia abrunebs options da ara
             chveulebrivad dates amitomac Some keywordit option shi dawrapavt dareturnebul values.kodis bolo line zustad am or ragacas
             aketebs *)



      (*assoc lists*)
  let insert k v list = (k,v) :: list ;;
  
  let rec lookup k list = 
    match list with
    |[] -> None
    |(k1,v)::t -> if k1 = k then Some v else lookup k t

    let assoclist = insert 3 "three" (insert 2 "two" (insert 1 "one" []));;

  let z = lookup 2 assoclist ;;
  let x = lookup 4 assoclist ;;

  
  (*Cards*)
type suit = Spade|Heart|Diamond|Club ;;
type rank = Number of int|Jack|Queen|King|Ace;;
type card = {suit : suit;rank : rank};;

let ace_of_clubs = {suit = Club ;rank = Ace};;
let queen_of_hearts = {suit = Heart ;rank = Queen};;
let two_of_diamonds = {suit = Diamond ;rank = Number 2};;

(*quadrant*)

type quad = I | II | III | IV
type sign = Neg | Zero | Pos

let sign (x:int) : sign =
  if x > 0 then Pos else if x=0 then Zero else Neg 

let quadrant : int*int -> quad option = fun(x,y) -> 
  match sign x, sign y  with
    | Pos,Pos -> Some I
    | Neg,Pos -> Some II
    | Neg,Neg -> Some III
    | Pos,Neg -> Some IV
    | _ -> None

    (*quadrant when*)

    let quadrant_when : int*int -> quad option = function
    | x,y when x>0 && y>0 -> Some I
    | x,y when x<0 && y>0  -> Some II
    | x,y when x<0 && y<0  -> Some III
    | x,y when x>0 && y<0  -> Some IV
    | _ -> None



    (*depth*)
   

    type 'a tree = 
    |Leaf 
    |Node of 'a * 'a tree * 'a tree


    let rec depth tree= 
    match tree with
      |Leaf -> 0
      |Node (_,left,right) -> 1 + max (depth left) (depth right)




    (*shape*)

    let rec shape tree1 tree2 = 
      match tree1,tree2 with
      |Leaf,Leaf -> true
      |Node(_,l1,l2),Node(_,r1,r2) -> (shape l1 l2) && (shape r1 r2)
      |_ -> false 

     

    (*list max exn*)

(*es helper funqcia or elements adarebs da mandedan maximums igebs.listis bolodan moyveba sanam bolomde ar amova rac base case aris.*)
    let rec list_max x = function
    | [] -> x
    | h::t -> list_max (Stdlib.max x h) t
 

  let list_max_exn = function
    | [] -> failwith "list_max"
    | h::t -> list_max h t


    (*list max exn String*)
    let list_max_string lst =
      try string_of_int (list_max lst) with
      | Failure _ -> "empty"


      (**)


  
