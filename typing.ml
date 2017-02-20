(* Typechecking of source programs *)

open Lang
open Analyses

(* Environments *)

type environment = 
    {localvar: (vname * tp) list; 
     globalvar: (vname * tp) list; 
     returntp: tp;
     funbind: fundecl list}


(* TODO: put your definitions here *)
let tp_prog (Prog (gvds, fdfs)) =
  Prog([],
       [Fundefn (Fundecl (BoolT, "even", [Vardecl (IntT, "n")]), [], Skip)])
;;


(* retrieve the type of vname *)
let retrieve_type x = function env ->
    let rec retrieve_type_aux x = function 
     	[] -> failwith"error"
     	|(a, b)::q -> if x=a then b else retrieve_type_aux x q
    in let localvar = env.localvar in retrieve_type_aux x localvar ;;	
(* val retrieve_type : vname -> environment -> tp = <fun> *)

let verifConst = function
	BoolV _ -> BoolT
	|IntV  _ -> IntT
	|VoidV -> VoidT;;

	let retrieve_val_aux = function 
	Var(_, v) -> v;;

let retrieve_val_aux = function 
	Var(_, v) -> v;;

let retrieve_val env = function
	Const(_, v) -> verifConst v
	|VarE (_, v) -> retrieve_type(retrieve_val_aux v) env
	|BinOp(a, binop, exp1, exp2) -> retrieve_binop env (binop, exp1, exp2) ;;
(* val retrieve_val : environment -> 'a expr -> tp = <fun> *)


let rec retrieve_binop env = function 
	 BArith x, exp1, exp2 -> let val1 = retrieve_val env exp1 in let val2 = retrieve_val env exp2 in 
								if val1=IntT && val2=IntT then IntT else failwith"error"
	|BCompar x, exp1, exp2 -> let val1 = retrieve_val env exp1 in let val2 = retrieve_val env exp2 in 
								if val1 = val2 then BoolT else failwith"error"
	|BLogic x, exp1, exp2 -> let val1 = retrieve_val env exp1 in let val2 = retrieve_val env exp2 in 
								if val1 = BoolT && val2 = BoolT then BoolT else failwith"error";;
	


(* retrieve the type of expressions *)
let rec tp_expr env = function
	Const(0, v) -> Const(verifConst v, v)
	|VarE (0,v)-> VarE((retrieve_type(retrieve_val v) env), v)
	|BinOp (0, binop, exp1, exp2)-> BinOp(retrieve_binop env (binop, exp1,exp2), binop, tp_expr env exp1, tp_expr env exp2);;


