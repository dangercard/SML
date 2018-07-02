open Math ;

datatype 'a option = NONE | SOME of 'a  ;


fun ConsOpt(a,NONE) = NONE
    | ConsOpt(a, SOME(l))  = SOME(a::l) ;

 fun isOdd(a) = if a mod 2 = 0 then false else true ;




fun prefix(f, SOME(nil)) = NONE
    | prefix( f, SOME(h::t) ) = if f(h)
                                then ConsOpt(h,prefix(f, SOME(t) ))
                                else SOME nil ;


(* continuation version *)

prefix'(f, SOME(nil), cont) = NONE
    | prefix'(f, SOME(h::t), cont) = if f(h) then ConsOpt(h,prefix(f,SOME(t), fn f => cont(h::r)))
