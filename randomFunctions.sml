              (* Functions defined during High Level Languages *)


                                (* IMPORTS *)
open Int ;
open Math ;
open String ;


                            (* Defined Datatypes *)

datatype Grade = Alpha| Bravo| Cool| Derrible ;

datatype 'a option = NONE | SOME of 'a ;

datatype 'a tree = Empty | Node of 'a tree * 'a * 'a tree ;

datatype 'a vtree = Empty | Node of 'a *('a tree list) ;

datatype 'a tree2 = Empty | Node of 'a * 'a forest and 'a forest = nil
          | Cons of 'a tree * 'a forest ;

datatype 'a tree3 = Empty | Node of 'a edge * 'a edge
          and 'a edge = Branch of 'a * 'a tree ;

datatype number = Int of int | Real of real;
(*
datatype expr = Number of int
          | plus of (expr + expr)
          | times of (expr * expr) ;
*)


                            (* Datatype Functions *)

fun height Empty = 0
          | height Node(left,_,right) = 1 + max(height left, height right) ;

fun size Empty = 0
          | size Node(left,_,right) = 1 + size(left) + size(right) ;


fun size2 Empty = 0
          | size Node (_,forest) = 1 + sizeF(forest) and fun sizeF Nil = 0
          | sizeF Cons(t,f) = size(t) + size(f) ;

fun collect Empty = nil
            | collect Node(Branch(lv,lt), Branch(rv,rt)) = lv::rv::(collect(lt)
            @ collect(rt)) ;

fun Eval Number n = n
        | Eval Plus(e1, e2) = Eval(e1) + Eval(e2)
        | Eval Times(e1,e2) = Eval(e1) * Eval(e2) ;

fun outrank (Alpha, Alpha) = false
            | outrank (Alpha, _ ) = true ;


fun VIP NONE = "Koutis"
        |VIP (SOME x) = "Student" ;





(*############################################################################*)

                        (** RECURSIVE DEFINITIONS **)


(* Fibbonacci Definitions. *)

fun fibb 0 = 1 (* n^2 *)
        | fibb 1 = 1
        | fibb n = fibb(n - 1) + fibb(n - 2) ;


fun fibb_h 0 = (1,0) (* Linear *)
          | fibb_h 1 = (1,1)
          | fibb_h n =
            let
              val (a,b) = fibb_h (n-1)

            in
              (a + b, a)

            end ;

(* Exponential Definitions. *)

fun exp 0 = 1 (* Linear *)
      | exp n = exp(n-1) * 2 ;

fun square n = n*n ;
fun double n = n + n ;

fun fast_exp 0 = 1 (* Logarithmic *)
            | fast_exp n =
              if n mod 2 = 0
              then square(fast_exp(n div 2))
              else double(fast_exp(n - 1)) ;

fun gexp (b, 0, a) = a (* Generalized fast_exp *)
        | gexp (b, n, a) =
          if n mod 2 = 0
          then gexp (b*b, n div 2, a)
          else gexp (b, n-1, a*b) ;

                          (* List Examples *)

(* h::l *)  (* h element of type 'a, l list of elements of type 'a *)

fun length nil = 0 (* Polymorphic *)
          | length(_::t) = 1 + length(t) ;


fun append (t,nil) = t
          | append (nil,t) = t
          | append (h::t , l) = h::(append(t,l)) ;


fun rev_helper (nil,t) = t
        | rev_helper (h::t, l) = rev_helper(t, h::l) ;


fun map' (f, nil) = nil
        |map'(f,h::t) = f(h)::(map' (f,t))

(*############################################################################*)


                                (* MISC *)

fun id x = x ;
fun idp x:'a = x ; (* Polymorphic example *)

fun (constantly k) a = k

fun curry f x y = f(x,y) ; (* ('a*'b  -> 'c) -> ('a -> ('b ->' c)) *)
(*We can make curry versions of existing functions*)

fun reduce (unit,opn, nil) = unit
          | reduce (unit, opn, h::t) = opn(h, reduce(unit,opn, t)) ;

fun betterReduce (unit, opn, l) =
                let fun red nil = nil
                        | red h::t = op(h, red t)

                in
                  red l

                end ;



(*############################################################################*)
