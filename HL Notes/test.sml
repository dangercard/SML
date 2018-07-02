
fun Hlfun(nil,m,f) = m
    |Hlfun(h::t,m,f) = Hlfun(t,m@[f(h)], f) ;

fun lfun(nil,_) = nil
    | lfun(l, f) = Hlfun(l,nil,f) ;

fun add(p) = p+1 ;




fun ins(a, b, l, elem) = a@[elem]@[b]@l ;


fun insert_h(nil,s,l, elem) = l@[s@[elem]]
    | insert_h(h::t,s, l, elem) = insert_h(t,s@[h],l@[ins(s, h, t, elem)], elem) ;


fun insert(nil,elem) = nil
  | insert(l,elem) = insert_h(l,nil,nil,elem) ;
