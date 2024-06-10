(*tail of the list*)

let rec tail_of_list list = 
  match list with
  |[] -> failwith "Damn"
  |[x] -> x
  |h::t -> tail_of_list t 

  (*last two elements of the list*)

let rec two_tails list = 
    match list with
    |[] -> failwith "Empty asf"
    |[x;y] -> x,y
    |h::t -> two_tails t 
 
    
  (*nth element of a list*)


let rec nth_element list n = 
    match n,list with
    |_,[] -> failwith "no element at that index"
    |0,h::t -> h
    |n,h::t -> nth_element list (n-1);;   


    (*es sxvanairadac sheidzleba gaaketo.as you can see i fucking loved this problem*)

let rec nth_element2 list n =
      match list with
      |[] -> -1
      |h::t -> if n = 0 then h else nth_element2 t (n-1) 


      let rec nth_element3 list n =
       match n with
      |0 -> (match list with 
            |[] -> failwith "null" 
            |h::t -> h)
      |_ -> (match list with 
            |[] -> failwith "null again"
            |h::t -> nth_element3 t (n-1))


(*the length of a list*)

let rec list_length list = 
  match list with
  |[] -> 0
  |h::t -> 1 + list_length t

 

  (*reverse a list*)


  let rec reverse_list list = 
    match list with
    |[] -> []
    |h::t -> reverse_list t @ [h] (*chveulebrivad pirvel elements igeb da ukan abav rekursiulad*)

  

  (*palindrome**)

  let palindrome list = 
    list = reverse_list list

    let rec nth_element list n = 
      match list with
      |[] -> -1
      |h::t -> 
        if n = 0 then h else nth_element t (n-1)

  (*Run-Length coding*)

  let encode list = 
    let rec aux count acc = function
    |[] -> []
    |[x] -> (count + 1,x) :: acc
    |a :: (b :: t1 as t) -> if a = b then aux  (count+1) acc t else 
                            aux 0 ((count+1,a) :: acc) t  in 
                  List.rev (aux 0 [] list);;  

  (*Duplicate elements of a list*)

  let rec duplicate list = 
    match list with
    |[] -> []
    |h::t -> h::h::(duplicate t)

  


  (*split the list into two parts*)

  let split list n  =
    let rec aux acc n list = 
      match list with
      |[] -> List.rev acc,[]
      |(h::t) as l -> if n = 0 then List.rev acc,l else aux (h::acc) (n-1) t
    in aux [] n list
  




   (*Remove the nth element of the list*)
   (*Both tail and non-tail recursion way*)

   let rec remove list n = 
    match list with
    |[] -> []
    |h::t -> if n <> 0 then h :: remove t (n-1) else remove t (n-1) 


    let remove1 list n =
      let rec aux acc n list = match list with
      |[] -> List.rev acc
      |h::t -> if n = 0 then aux acc (n-1) t else aux (h::acc) (n-1) t 
    in aux [] n list 
   

    (*Insert an element at a given position into a list*)
  

    let rec insert_at x n list = 
      match list with
      |[] -> []
      |h::t -> if n = 0 then h :: x :: t else h :: insert_at x (n-1) t

    
      let  insert_at1 x n list = 
        let rec aux x n list acc = 
          match list with
          |[] -> List.rev acc
          |h::t -> if n = 0 then aux x (n-1) t (h::x :: acc) else aux x (n-1) t (h ::acc) 
        in aux x n list []


      (*Create a List Containing All Integers Within a Given Range*)

    let rec range x y = 
      let rec aux x y acc = 
        if x > y then [] else aux (x+1) y (x::acc)
      in aux x y [] 
