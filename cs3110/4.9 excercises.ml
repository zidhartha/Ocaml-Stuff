(*repeat*)

let rec repeat f n x = 
 if(n>0) then repeat f (n-1) (f x) else x ;;

(*product*)
 let product_left list = List.fold_left ( *. ) 1.0 list

 let product_right list = List.fold_right ( *. ) list 1.0
let x = product_left [2.;3.;4.;5.;6.;7.;8.;9.;10.];;


(*sum_cube_odd*)
(*First we need a function that takes all integers to n into a list*)


let rec to_list n list  = 
   match n with
   |0 -> list
   |n -> to_list (n-1) ([n] @ list) 



let some_cube_odd n = 
   let z = to_list n [] in
   let x = List.filter (fun z -> z mod 2 = 1) z in
   let y = List.map(fun x -> x*x*x) x in 
   List.fold_left (+) 0 y 


let x = some_cube_odd 3;;




let some_cube_odd_pipeline n = 
   to_list n [] 
   |> List.filter (fun i -> i mod 2 = 1)
   |> List.map(fun x -> x*x*x)
   |> List.fold_left (+) 0
   
   

   let z = some_cube_odd_pipeline 3;;

(*exists*)

let rec exists_rec x lst = 
   match lst with
   |[] -> false
   |h::t -> (x = h) || (exists_rec x t) 


let exists_fold x lst = 
   List.fold_left (fun acc elt ->acc || x = elt) false lst 


let exists_lib x lst = List.exists x lst 

(*idk this already exists so why we wrote it mind boggling to me*)



(*account balance*)

let balance_left balance debits = 
   List.fold_left ( -.) balance debits 


let balance_right balance debits =
   List.fold_right (fun b d -> b -. d) debits balance

let rec balance_rec balance debits = 
   match debits with
   |[] -> balance
   |h::t -> balance_rec (balance -.h) t 

(*library uncurried*)

let append (list1,x) = List.append list1 x ;;

let compare_chars (char1,char2) = Char.compare char1 char2 ;;

let max (x,y) = Stdlib.max x y ;;



(*map composition*)

let superior_map f g list = 
   List.map (fun acc -> f (g acc)) list 



(*more list fun*)

let length_of_three list = 
   List.filter (fun x -> String.length x = 3) list 


let  add_one list = 
List.map (fun x -> x +. 1.0) list 



let join list str = 
   match list with
   |[] -> ""
   |h::t -> 
      List.fold_left (fun x y  -> x^str^y) h t



let association_list = [("square",4);("triangle",3);("icosagon",20)];;

let rec keys list = 
   match list with
   | [] -> []
   |(k,v) :: t -> k :: keys t  


let unique_keys list =
   List.sort_uniq compare (keys list)

   (*while the first implementation above is an easy one.it is also very space and time consuming complexity wise.
      We could have written it in one line or so*)

   let unique_keys1 list = 
      List.fold_right (fun (k,_) acc -> k :: List.filter (fun k1 -> k1<> k) acc ) list [] ;;

let unique_keys2 list = 
   List.fold_left (fun acc (k,_) -> if List.exists ((<>)k) acc then k::acc else acc ) [] list 


   (*Valid matrix*)

   let is_valid_matrix list = 
      match list with
      |[] -> false
      |r :: rows ->
         let m = List.length r in 
          m>0 && List.for_all (fun r -> m = List.length r) rows 

   

(*row vector add*)

let rec add_row_vectors list1 list2 = 
   match list1, list2 with
   | [], [] -> []
   | [], _ | _, [] -> failwith "Lists must be of the same length"
   | h1 :: t1, h2 :: t2 -> (h1 + h2) :: add_row_vectors t1 t2
let c =  add_row_vectors [1;2;3] [4;5;6;8];;



(*Matrix add*)
let matrix_addition list1 list2 = 
   List.map2 add_row_vectors list1 list2 ;;

let x = matrix_addition [[1;2;3];[4;5;6]] [[7;8;9];[10;11;12]];;

(*Matrix multiplication*)

let rec transpose ls =
   let rec transpose' acc = function
     | [] | [] :: _ -> List.rev acc
     | ls -> transpose' (List.map List.hd ls :: acc) (List.map List.tl ls)
   in transpose' [] ls
 
 let dot = List.fold_left2 (fun acc x y -> acc + x * y) 0
 
 let multiply_matrices m1 m2 =
   List.map (fun row -> List.map (dot row) (transpose m2)) m1































(*Some code i wrote while reading this chapter.*)


let rec map f = function
|[] -> []
|h::t -> f h :: map f t  

let listToString list = map string_of_int list;;

let list = [2;4;5;6;7;8];;

List.map(function x -> x ^ "s") ;;



let rec filter p = function
|[] -> []
|h::t -> begin 
   if p then h:: filter p t else filter p t 
end


let rec fold_right acc f = function 
|[] -> acc
|h::t -> f h (fold_right acc f t)


let rec fold_left acc f list = match list with
|[] -> acc
|h::t -> 
   let acc' = f acc h 
in fold_left acc' f t 



type 'a tree = 
|Leaf
|Node of 'a * 'a tree * 'a tree 

let t = Node(1,
        Node(2,Leaf,Leaf),
        Node(3,Leaf,Leaf));;

let rec map f = function
|Leaf -> Leaf
|Node (v,l,r) -> Node(f v,map f l, map f r )


let add1 t = map succ t ;;


let rec fold acc f = function
|Leaf -> acc 
|Node (v,l,r) -> f v (fold acc f l) (fold acc f r)



let sum = fold 0 (fun x y z -> x + y + z) t ;;


let t1 = add1 t  ;;



let namravli = fold 1 ( fun x y z -> x * y * z) t1 ;;

let preoder t = fold [] ( fun x l r -> [x] @ l @ r ) t;;


let rec count_Leaves t = match t with
|Leaf -> 1 
|Node(v,l,r) -> (count_Leaves l) + (count_Leaves r)  


let example_tree =
   Node (1, 
     Node (2, Leaf, Leaf), 
     Node (3, 
       Node (4, Leaf, Leaf), 
       Leaf))


       let goat = count_Leaves example_tree;;
      

