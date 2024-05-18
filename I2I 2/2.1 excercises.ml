(*operators*)
let x = 42 * 10;;

let z = 3.14 /. 2.;;

let rec exponent x y = match y with
|0. -> 1.
|_ -> x *. exponent x (y -. 1.);;

let m = exponent 4.2 7.;;

(*assert*)
assert true;;


assert (2110 <> 3110);;

(*if the assertion is true then the code continues running but if it is false then the code throws an exception.
   that is the idea of the assert function*)


(*if*)

  if 2 > 1 then 42 else 7;;


  (*double fun*)

  let double x = x * 2;;
  let z = double 7;;

  let _ = assert(double 7 = 14);;
  let _ = assert(double (-4) = -8);;
  let _ = assert(double 14 = 28);;

  (*all of these are true therefore the function is correct*)


  (*more fun*)

  let cube x = x ** 3.;;
  let y = cube 3.;;


  let sign x = if x > 0 then 1 else if x = 0 then 0 else -1;;
  let x = sign 12;;

let area x = 3.14 *. (x*.x);;

let _ = assert(area 5. = 78.5)
let _ = assert(area 2. = 12.56)


(*RMS*)

let rms x y =  sqrt((x *. x +. y *. y)/. 2.);;

let _ = assert(rms 5. 5. = 5.);;


(*Date fun*)


let date (d : int) (m : string) = 
  if m = "Jan" || m = "Mar" || m = "May" || m = "Jul" 
    || m = "Aug" || m = "Oct" || m = "Dec"
    then 0 < d && d <= 31
else if  m = "Apr" || m = "Jun" || m = "Sep" || m = "Nov"
  then 0 < d && d <= 30
else if m = "Feb" 
  then 1<=d && d<=28
else false;;

(*mtliani am kodis ideaa sheamowmos jer tve tu aris swori da shedeg am tvis shesabamisad amowmebs tu dgeebi emtxveva.*)



(*fib*)

let rec fib x = match x with
|0 -> 1
|1 -> 1
|_ -> x * fib (x-1);;

let _ = assert(fib 6 = 720);;


(*fib fast*)

(*just a fancy way of calculationg fibonacci sequence but also faster for bigger numbers*)
let rec h n pp p = match n with
| 1 -> p
|_ -> h (n-1) p (pp+ p)


let func fib_fast n = match n with
|0 -> 0
|_ -> h n 0 1



(*divide*)

let  divide numerator denominator = numerator /. denominator;;

let divide1 ~numerator:n ~denominator:d = n /. d;; 


let c = divide1 4. 5.;;

(*average*)

let (+/.) x y = (x +. y)/.2.;;

let _ = assert(1.0 +/. 3.0 = 2.0);;

(*Hello World!*)


print_endline "Hello World!";;
print_string "Hello World!";;