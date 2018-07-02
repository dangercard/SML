type id = string

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
	     | AssignStm of id * exp
	     | PrintStm of exp list

     and exp = IdExp of id
	     | NumExp of int
             | OpExp of exp * binop * exp
             | EseqExp of stm * exp
val prog =
 CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
  CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a", Minus,NumExp 1)],
           OpExp(NumExp 10, Times, IdExp"a"))),
   PrintStm[IdExp "b"]))


fun StmPrint (CompoundStm(S1,S2)) = (StmPrint S1 ;
 																		print ";" ;
																		StmPrint S2 )
	| StmPrint (PrintStm[]) = print "\n"
	| StmPrint (PrintStm(x::xs)) = (print "print" ; printExp x) (* Llama a printExp para bregar con *)
	and printExp (NumExp n) = print (Int.toString n) ; (* Def de printExp para imprimir numeros*)


StmPrint(CompoundStm(PrintStm[NumExp 1], PrintStm [NumExp 2])) ;
