

let rec interleave3 l1 l2 l3 =
  let rec interleave2 l1 l2 =
    match l1 with [] -> l2
    | x::xs -> x::interleave2 l2 xs
  in
  match l1 with [] -> interleave2 l2 l3
  | x::xs -> x::interleave3 l2 l3 xs


(*****************************************************************************)
(* Assignment 5.7 [2 Points] *)
let foo x y b =
  let x,y = if x > y then y,x else x,y in
  let rec loop x y b =
    if x >= y then x
    else if b then loop (x+1) y (not b)
    else loop x (y-1) (not b)
  in
  loop x y b


(*****************************************************************************)
(* Assignment 5.8 [4 Points] *)
let eval_poly x coeffs =
  let rec impl value coeffs =
    match coeffs with [] -> value
    | c::cs -> impl ((value *. x) +. c) cs
  in
  impl 0.0 coeffs

let derive_poly coeffs =
  let rec impl = function [] | [_] -> ([], 0.)
  | c::cs -> let (new_coeffs, deg) = impl cs in
    (c *. (deg +. 1.))::new_coeffs, deg +. 1.
  in fst (impl coeffs)


(*****************************************************************************)
(* Assignment 5.9 [6 Points] *)
let lt_seq l =
  (* compare the first up to n elements of l1 and l2 to find the longest common prefix, not longer than n *)
  let rec longest_prefix n l1 l2 =
    if n <= 0 then [], 0 else
    match l1, l2 with x::xs, y::ys ->
      if x <> y then [], 0 else
      let l,n = longest_prefix (n-1) xs ys in
      (x::l, n+1)
    | _ -> [], 0
  in
  (* iterate through the list l2 and compare it with l1 *)
  let rec iter_l2 n l1 l2 (best_l, best_n) =
    match l2 with [] -> (best_l, best_n)
    | y::ys -> let pre_l, pre_n = longest_prefix n l1 l2 in
      iter_l2 (n+1) l1 ys (if pre_n > best_n then (pre_l, pre_n) else (best_l, best_n))
  in
  (* iterate through the list l1 *)
  let rec iter_l1 l1 (best_l, best_n) =
    match l1 with [] -> best_l
    | x::xs -> iter_l1 xs (iter_l2 1 l1 xs (best_l, best_n))
  in iter_l1 l ([], 0)