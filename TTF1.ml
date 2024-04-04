let rec fact x = 
  match x with
  |0 -> 1
  |1 -> 1
  |_ -> fact(x-1) * x;;


  let max a b=
  if(a>b) then a
  else b;;


let z = fact 50;;

let rec pow z x= 
  match z,x with
  |0,_ -> 0
  |_,0 -> 1 
  |z,x -> z * pow z (x-1);;

  let ab = pow 2 3;;
