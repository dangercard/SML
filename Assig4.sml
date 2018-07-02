(*
   Alejandro Deloach Rivera
   801-13-1900
   Prof. Koutis
*)


(* Rotate Helper Functions *)
fun Last([a]) = a
    | Last(h::t) = Last(t) ;

fun First(h::t) = h ;

fun RemoveFirst(h::t) = t ;


fun RemoveLast([a], n) = n
    | RemoveLast(h::t,n) = RemoveLast(t,n@[h]) ;


(* 1. *)
fun Rotate(l,0) = l
    | Rotate(nil,_) = nil
    | Rotate(l,n) = if n > 0
                    then Rotate([Last(l)]@RemoveLast(l,nil), n-1)
                    else Rotate(RemoveFirst(l)@[First(l)], n+1) ;


(* Divide Helper Function *)
fun Div_helper(l,s,0) = (s,l)
    | Div_helper(h::t, s, k) = Div_helper(t,s@[h], k-1) ;

(* 2. *)
fun Divide(nil,_) = (nil,nil)
  | Divide(l,k) = Div_helper(l,nil,k) ;





(* Drop Helper Function*)
fun Drop_helper(nil,l,n,k) = l
    |Drop_helper(h::t, l, n, k) = if k mod n = 0
                                  then Drop_helper(t,l,n,k+1)
                                  else Drop_helper(t,l@[h],n,k+1) ;
(* 3. *)
fun Drop(nil,_) = nil
    | Drop(l,n) = Drop_helper(l,nil,n,0) ;





(* FibbDrop Helper Functions *)
fun fibb 0 = 1 (* n^2 *)
        | fibb 1 = 1
        | fibb n = fibb(n - 1) + fibb(n - 2) ;

fun FibbDrop_helper(nil,l,k,n) = l
    | FibbDrop_helper(h::t, l, k,0) = FibbDrop_helper(t,l,k+1,1)
    | FibbDrop_helper(h::t, l, k,1) = FibbDrop_helper(t,l,k+1,2)
    | FibbDrop_helper(h::t, l, k,n) = if k = fibb(n)
                                      then FibbDrop_helper(t,l, k+1,n+1)
                                      else FibbDrop_helper(t,l@[h], k+1,n) ;

(* 4. *)
fun FibbDrop nil = nil
    | FibbDrop(s) = FibbDrop_helper(s,nil,0,0) ;


(* 5 *)

(* a. *)
datatype 'a tree = Leaf of 'a
          | Node of 'a tree * 'a tree ;


(* b. *)
fun LeafCollect (Leaf elem) = [elem]
    | LeafCollect (Node(left,right)) = LeafCollect(left)@LeafCollect(right) ;








fun Insert(n,'a tree)
    | Insert(n, Node(lef
