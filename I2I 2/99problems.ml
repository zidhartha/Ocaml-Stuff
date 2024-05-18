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


  


  
