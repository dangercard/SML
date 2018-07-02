(* Alejandro Deloach
   801-13-1900
   Prof. Koutis
*)

open Int ;
open Math ;

(* 1a *)
fun Sum nil = 0 (* non tail-recursive.*)
    | Sum (h::t) = h*h + Sum(t) ;

(* 1b *)

fun SumT (nil,l) = l
    | SumT(h::t,l) = SumT(t,l + h*h) ;

fun Sum'(list) = SumT(list,0) ; (* Tail-Recursive *)


(* 2a *)
fun Min nil = 0
    | Min (h::t) = if h <= Min(t) then h else Min(t) ;

fun Max nil = 0
    | Max (h::t) = if h >= Max(t) then h else Max(t) ;




(* 4 *)


fun append' (t,nil) = t
          | append' (nil,t) = t
          | append' (h::t , l) = h::(append'(t,l)) ;

fun appendTwo ([a], l ) = append'(l, a)
            | appendTwo(nil, l) = nil
            | appendTwo (h::t,l) = append'(l,append'(h,appendTwo(t,l))) ;

fun Append(list) = appendTwo(list,nil) ;  (* Call this one *)



(* 5 *)
(* Out of Service
fun 3D (nil,nil) = nil
    | 3D(nil,l) = l
    | 3D(h::t,l) = if sqrt(real(Sum(h))) > 3D(t,l@h) then h else 3D(t,l) ;
*)
