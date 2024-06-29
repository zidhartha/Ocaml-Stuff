module MyStack = struct
  type 'a stack = 
  |Empty
  |Entry of 'a * 'a stack

  let empty = Empty

  let push x s =
    Entry (x,s)

  let peek = function
     |Empty -> failwith "Stack is empty"
     |Entry (x,_) -> x

  let pop = function
  |Empty -> failwith "Stack is empty"
  |Entry(_,x) -> x
end 


module ListStack = struct
  type 'a stack = 'a list 

  let empty = []

  let push x s = 
    x :: s

  let peek = function
  |[] -> failwith "Stack is empty"
  |h::t -> h


  let rec pop = function
  |[] -> failwith "Stack is empty"
  |_ :: t -> t
end


let s = ListStack.empty;;
let s' = ListStack.push 1 s ;;
let x = ListStack.peek s;;