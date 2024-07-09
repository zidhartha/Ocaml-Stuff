(*Complex Synonym*)
module type ComplexSig = sig
  type t = float * float 
  val zero : t
  val add : t -> t -> t
end

(*complex encapsulation*)

module Complex : ComplexSig = struct
  type t = float * float
  let zero = (0., 0.)
  let add (r1, i1) (r2, i2) = r1 +. r2, i1 +. i2
end

(*Every error happens because of signature mismatch since module needs to have the exact same  types as signature*)


(*Big list queue*)


(** Creates a ListQueue filled with [n] elements. *)
module ListQueue = struct 
  type 'a queue = 'a list 

  let empty = []

  let enqueue x q = 
   [x] @ q

 
  let peek q = 
     match q with
     | h :: t -> Some h
     |[] -> None

  let dequeue q = 
     match q with
     |[] -> None
     |h :: t -> Some t 
end


let fill_listqueue n =
  let rec loop n q =
    if n = 0 then q
    else loop (n - 1) (ListQueue.enqueue n q) in
  loop n ListQueue.empty

  let z = fill_listqueue 100 ;;
  (*idk my leptop doesnt really slow down that much to the point of 10 second delay*)



  (*Batched queue*)

  module BatchedQueue = struct 
  
    type 'a queue = {
      front : 'a list;
      back : 'a list;
    }
  
  
    let empty = {
      front = [];
      back = [];
    }
  
    let peek = function 
       |{front = []} -> None
       |{front = h :: t } -> Some h
  
  
    let enqueue x = function
       |{front = []} -> {front = [x];back = []}
       |q -> {q with back = x :: q.back} (*Since the back is in reverse order we need to put the element on top of the back list*)
    
  
    let dequeue x = function 
    |{front = []} -> None
    |{front = h :: [];back } -> 
      Some {front = List.rev back; back = []}  (*If the front list becomes empty, it becomes the reversed back list.*)
    |{front = _ :: t ; back} -> Some {front = t;back}
    
   end
   

   let fill_batchedqueue n =
    let rec loop n q =
      if n = 0 then q
      else loop (n - 1) (BatchedQueue.enqueue n q) in
    loop n BatchedQueue.empty

  
    let z = fill_batchedqueue 5000000;;

(*Binary search tree map*)


module type Map = sig 
    type ('k, 'v) t
    val empty  : ('k, 'v) t
    val insert : 'k -> 'v -> ('k,'v) t -> ('k,'v) t
    val lookup  : 'k -> ('k,'v) t -> 'v
  end



module BSTmap : Map = struct 
  type 'a tree = 
  |Leaf
  |Node of 'a * 'a tree * 'a tree
  
  type ('k, 'v) t = ('k * 'v) tree

  let empty = Leaf

  let rec insert k v = function
  | Leaf -> Node((k, v), Leaf, Leaf)
  | Node ((k',v'), l, r) ->
    if (k = k') then Node ((k, v), l, r)
    else if (k < k') then Node ((k',v'), insert k v l, r)
    else Node ((k',v'), l, insert k v r)

let rec lookup k = function
  | Leaf -> failwith "Not_found"
  | Node ((k',v'), l, r) ->
    if (k = k') then v'
    else if (k < k') then lookup k l
    else lookup k r
end


(*Fraction*)


module type Fraction = sig
  (* A fraction is a rational number p/q, where q != 0. *)
  type t

  (** [make n d] is n/d. Requires d != 0. *)
  val make : int -> int -> t

  val numerator : t -> int
  val denominator : t -> int
  val to_string : t -> string
  val to_float : t -> float

  val add : t -> t -> t
  val mul : t -> t -> t
end

module Fraction_module = struct 

  type t = int * int 

  let make n d = 
    if (d!=0) then (n,d) else (0,-1)

  let numerator (n,d) = n
  let denominator (n,d) = d

  let to_string (n,d) =  string_of_int n ^ " / " ^ string_of_int d

  let to_float (n,d) = n /. d 


  let add (n1,d1) (n2,d2) = 
    (n1 * d2 + n2 * d1,d1 * d2)

  let mul (n1,d1) (n2,d2) =
    (n1*n2,d1*d2) 
end


(*Fraction reduced*)
let rec gcd a b =
  if b = 0 then
    a
  else
    gcd b (a mod b)

module FRACTION_REDUCED = struct

  let make n d = 
    if gcd n d = 1 then (n,d) else (n/(gcd n d),d/(gcd n d))
  
    let add (n1, d1) (n2, d2) = 
      let d' = ((d1 * d2)/ (gcd d1 d2)) in
      let n' = (n1 * (d' / d1)) + (n2 * (d' / d2)) in
      (n', d')
end

(*Char Map*)


module CharMap = Map.Make(Char)

 let map = CharMap.(
  empty
  |> add 'A' "Alpha"
  |> add 'B' "Betha"
  |> add 'S' "Sigma ;))"
  |> add 'V' "Victor"
 )

let echo = CharMap.find 'E' map 
let map' = CharMap.remove 'A' map
let a_exists = CharMap.mem 'A' map'
let bindings = CharMap.bindings map'
                                         


(*Date order*)

type date = {month : int; day : int}


module Date = struct 
   type t = date 

   let compare t1 t2 =
    if t1.month = t2.month then t1.day - t2.day
    else t1.month - t2.month
end


(*Calendar*)

module DateMap = Map.Make(Date)

type calendar = string DateMap.t

let my_calendar =
  DateMap.(empty |>
           add { month = 1; day = 6 } |>
           add { month = 3; day = 4 }  |>
           add { month = 5; day = 18 }  (* according to some *) |>
           add { month = 12; day = 2 }  |>
           add { month = 11; day = 25 } 
          )


(*Print calendar*)


let print_calendar = 
  DateMap.iter (fun date x -> Printf.printf "%d/%d: %s\n" date.month date.day x)




(*Is for*)

let is_for map=
    CharMap.mapi (fun key v -> Printf.sprintf "%c is for %s" key v) map



  (*First after*)

  let first_after date cal = 
        DateMap.find_first (fun k -> (Date.compare k date) > 0 ) cal |> snd 


