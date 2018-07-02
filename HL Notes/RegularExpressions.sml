(*  REGULAR EXPRESSION: Define some range (Example: Alphabet)

                    1. if a is in the range, a is a regular expression

                    2. if r1 and r2 are regular expressions, then
                       r1r2 is a regular expression.

                    3. 0 is a regular expression.

                    4. if r1 and r2 are regualr expressions, then
                       r1 + r2 is a regualr expression.

                    5. 1 is a regular expression.

                    6. if r is a regular expression, then r* is a regualr
                       expression.

                  Example

                    R = (ba)* => L(R) = { {}, ba, baba, bababa, ....}



                  Tips:

                  L(a) = {a}

                  L(r1r2) = S1S2 such that S1 is in L(r1) and S2 is in
                            L(r2).

                  L(0) = {}.

                  L(a* + b) = L( a* ) U L(b).

                  L( r* ) = S1,S2,S3,...,Sn such that for all i Si is in L(r).



                  Examples:

                  R = (a + b)*aa(a+b)* = any combination of a and b with any
                                         combination of a and b with 2 a's in
                                         between.


                  R = ( (a + 1)(b + ba)* ) = a or empty string with any
                                             combination of b and ba.


                    *)

open String ;
open List ;


datatype regexp = Char of char
                | One
                | Zero
                | Times of regexp * regexp
                | Plus of regexp * regexp
                | Star of regexp

(*
fun acc : regexp -> Char list -> (char list -> bool) -> bool


fun accept r s = acc r (String.explode s) List.null
*)


fun acc (Zero) s k = false
    | acc (One) s k = k(s)
    | acc ( Char(c) ) (c1::s) k = if c = c1 then k(s) else false
    | acc ( Plus(r1,r2) ) s k = acc r1 s k orelse acc r2 s k
    | acc ( Times(r1,r2) ) s k = acc r1 s (fn s' => acc r2 s' k)
    | acc ( Star(r) ) s k = k s orelse acc r s (fn s' => acc(Star(r)) s' k) ;


fun accept r s = acc r (String.explode s) List.null ;


val r = Star(Char(#"a")) ;

val e1 = Times( Times( Char(#"a"),Char(#"b") ), Times(Star(Char(#"a")), Char(#"b"))) ;
val e1b = Times( Times(Times( Char(#"a"),Char(#"b")), Star(Char(#"a"))),Char(#"b")) ;

val e2 = Star(Plus(Char(#"a"),Char(#"b"))) ;
