(* Compilation functions *)

open Lang;;
open Analyses;;
open Instrs;;
(* ************************************************************ *)
(* **** Compilation of expressions / statements            **** *)
(* ************************************************************ *)



(* ************************************************************ *)
(* **** Compilation of methods / programs                  **** *)
(* ************************************************************ *)

let gen_prog (Prog (gvds, fdfs)) = 
  JVMProg ([], 
           [Methdefn (Methdecl (IntT, "even", [IntT]),
                      Methinfo (3, 1),
                      [Loadc (IntT, IntV 0); ReturnI IntT])])


(* find the position of x in a list *)
let rec position x = function
  	[] -> failwith"Not in the list"
  	|a::q -> if a=x then 0 else 1+position x q;;
(*  val position : 'a -> 'a list -> int = <fun> *)


(* take a Var and return it value *)
let varAux = function
  Var(_, a) -> a;;
(* val varAux  var -> vname = <fun> *)
 
(* generate bytecode *)
let rec gen_exp liste = function
   Const(t, v)-> [Loadc (t ,v)]
  |VarE (t,v)  -> [Loadv (t, position(varAux v) liste)]
  |BinOp (t, binop, exp1, exp2)-> (gen_exp liste exp1)@(gen_exp liste exp2)@([Bininst(t, binop)]);;
(* val gen_exp : vname list -> tp expr -> instr list = <fun> *)
