(* Continuations: functions that hand over parameters to another function to get the result*)
(* Basis for continuation:
fun function(parameters,cont function) = cont function(function(parameters,cont function))*)

fun add(x,y,k) = k(x+y) ;

fun mult(x,y,k) = k(x*y) ;

val direct = (3+4) * (5+6) ;

val bycont = add(3,4,fn r1 => add(5,6,fn r2 => mult(r1,r2,fn r3 => r3))) ;


fun app(nil,l) = l
    | app(h::t,l) = h::app(t,l) ;

fun app'(nil,l,cont) = cont l (* Tail Recurive *)
    | app'(h::t,l, cont) = app'(t ,l, fn r => cont(h::r)) ;
(* cont = fn r2 => (fn r1 => (fn r = r) 1::r1) 2::r2  *)

fun length(nil,cont) = cont(0)
    | length(h::t,cont) = length(t,fn r => cont(1 + r)) ;



(*
datatype arith = Plus of arith*arith
    | Mult of arith*arith
    | Divide of arith*arith
    | Value of int ;

exception Error ;

fun eval0(Value(n)) = n
    |eval0(Plus(a,b)) = eval0(a) + eval0(b)
    |eval0(Mult(a,b)) = eval0(a) + eval0(b)
    |eval0(Divide(a,b)) = if b = 0 then raise Error else eval0(a) div eval0(b) ;


fun eval'(Plus(a,b),cont) = eval'(a, fn r => eval'(b, fn r2 => cont(r + r2))) ;

fun evalDiv(Divide(a,b), cont) = evalDiv(a, fn r => evalDiv(b, fn r2 => if(r2 = 0) then NONE else cont(r div r2))) ;

BASE eval'(Value(n),n) = k(n) ;
*)
