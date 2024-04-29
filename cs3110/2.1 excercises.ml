let rms x y = sqrt((x *. x +. y*.y))/.2.;;

let _ = assert (rms 3. 4. = sqrt(12.5));;

