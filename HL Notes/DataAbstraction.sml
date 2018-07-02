open Math ;


structure IntLT = struct
  type t = int
  val lt = (op <)
  val eq = (op =)
end




structure IntDiv = struct
  type t = int
  fun lt(m,n) = (m mod n = 0)
  val eq = (op =)
end




structure Queue  =
struct
  type 'a queue = 'a list

  val empty: 'a queue = (nil)

  fun insert(x, l) = l@[x]

  exception Empty

  fun remove(nil) = raise Empty
    | remove(h::t) = t

end
