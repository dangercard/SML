
fun amount (a,n,nil) = [(a,n)]
  |amount(a,n,h::t) = if n = h then amount(a+1,n,t) else amount(a,n,nil) ;


fun compress_list nil = nil
  | compress_list (h::t) = amount(1,h,t)@compress_list(t) ;



(*
fun ch(nil, l) = (nil, l)
  | ch (h::t, (el,n)::t2) = if h = el then ch(t,(el,n+1)::t2 else ch(t,((h,1)::(el,n)::t2)) ;
*)

fun expand ((a,0), l) = l
  | expand((a,n),l) = expand((a,n-1), l@[a]) ;

fun decompress nil = nil
  | decompress(h::t) = expand(h,nil)@decompress(t) ;
