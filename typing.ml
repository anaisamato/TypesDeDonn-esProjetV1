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

(* Exercices 1,2 -> TP_EXPR *)
(* retrieve the type of vname in the environment*)
let retrieve_type x = function env ->
    let rec retrieve_type_aux x = function 
     	[] -> failwith"error"
     	|(a, b)::q -> if x=a then b else retrieve_type_aux x q
    in let localvar = env.localvar in retrieve_type_aux x localvar ;;	
(* val retrieve_type : vname -> environment -> tp = <fun> *)

(* check if a function is in the environment *)
let rec check_function name = function env ->
	let rec check_function_aux name = function
		[] -> failwith"error, function unknown"
		|Fundecl(t, n, []) ::l-> if name=n then Fundecl(t,n,[]) else check_function_aux name l
		|Fundecl(t, n, Vardecl(a,b)::q)::l-> if name=n then Fundecl(t,n, Vardecl(a,b)::q) else check_function_aux name l
	in let func= env.funbind in check_function_aux name func;;
(*  val check_function : fname -> environment -> fundecl = <fun>*)

(* transform a value in a type *)
let verifConst = function
	BoolV _ -> BoolT
	|IntV  _ -> IntT
	|VoidV -> VoidT;;
(* val verifConst :: value -> tp = <fun> *)

(* retrieve a vname with a var *)
let retrieve_var = function 
	Var(_, v) -> v;;
(* val retrieve_var : var -> vname = <fun> *)

(* retrive the type of a expression *)
let rec retrieve_val env = function
	Const(_, v) -> verifConst v
	|VarE (_, v) -> retrieve_type(retrieve_var v) env
	|IfThenElse(a, vIf, vThen, vElse) -> let valIf = retrieve_val env vIf in let valThen = retrieve_val env vThen in let valElse = retrieve_val env vElse in 
											if valIf = BoolT && valThen = BoolT && valElse = BoolT then BoolT else
												if valIf = BoolT && valThen = IntT && valElse = IntT then IntT else failwith "error"
	|BinOp(a, binop, exp1, exp2) -> match binop with
									BArith x -> let val1 = retrieve_val env exp1 in let val2 = retrieve_val env exp2 in 
										if val1=IntT && val2=IntT then IntT else failwith"error"
									|BCompar x -> let val1 = retrieve_val env exp1 in let val2 = retrieve_val env exp2 in 
										if val1 = val2 then BoolT else failwith"error"
									|BLogic x-> let val1 = retrieve_val env exp1 in let val2 = retrieve_val env exp2 in 
										if val1 = BoolT && val2 = BoolT then BoolT else failwith"error";;
(* val retrieve_val : environment -> 'a expr -> tp = <fun> *)

(* retrieve the type of a BinOP and check errors *)
let rec retrieve_binop env = function
	BArith x, exp1, exp2 -> let val1 = retrieve_val env exp1 in let val2 = retrieve_val env exp2 in 
								if val1=IntT && val2=IntT then IntT else failwith"error"
	|BCompar x, exp1, exp2 -> let val1 = retrieve_val env exp1 in let val2 = retrieve_val env exp2 in 
								if val1 = val2 then BoolT else failwith"error"
	|BLogic x, exp1, exp2 -> let val1 = retrieve_val env exp1 in let val2 = retrieve_val env exp2 in 
								if val1 = BoolT && val2 = BoolT then BoolT else failwith"error";;
(* val retrieve_binop : environment -> binop * 'a expr * 'b expr -> tp = <fun> *)

(* check the arguments of the CallE and the Vardecl of the Fundecl *)
let rec check_function_type = function
	Fundecl(t,_,[]), [] -> t
	|Fundecl(_,_,_), [] -> failwith"error"
	|Fundecl(_,_,[]), _ -> failwith"error"
	|(Fundecl(t,n,Vardecl(a,b)::q)), (Vardecl(c,d)::l) -> if a=c then check_function_type(Fundecl(t,n, q), l) else failwith"error";;
(* val check_function_type : fundecl * vardecl list -> tp = <fun> *)


(* transform a constant in a Varcecl *)
let rec transform_del_vardecl env = function
	[] -> []
	|(Const(a,IntV b))::q -> Vardecl(IntT, string_of_int b)::transform_del_vardecl env q
	|(Const(a,BoolV b))::q -> Vardecl(BoolT, string_of_bool b)::transform_del_vardecl env q;;
(* val transform_del_vardecl : 'a -> 'b expr list -> vardecl list = <fun> *)

(* check the type of Const in a list *)
let rec check_function_type_list env = function
	[] -> []
	|Const(a,b)::q -> Const(verifConst(b), b)::check_function_type_list env q;;
(* val check_function_type_list : 'a -> 'b expr list -> tp expr list = <fun> *)

(* retrieve the type of expressions *)
let rec tp_expr env = function
	Const(0, v) -> Const(verifConst v, v)
	|VarE (0,v)-> VarE((retrieve_type(retrieve_var v) env), v)
	|BinOp (0, binop, exp1, exp2)-> BinOp(retrieve_binop env (binop, exp1,exp2), binop, tp_expr env exp1, tp_expr env exp2)
	|IfThenElse(0, vIf, vThen, vElse) -> IfThenElse(retrieve_val env(IfThenElse(0,vIf, vThen, vElse)), tp_expr env vIf, tp_expr env vThen, tp_expr env vElse)
	|CallE(0, name, Const(a,b)::q) -> let func = check_function name env in let varD= transform_del_vardecl env (Const(a,b)::q) in 
										CallE((check_function_type(func, varD)), name, check_function_type_list env (Const(a,b)::q));;
(* val tp_expr : environment -> int expr -> tp expr = <fun> *)

let binop1 = BinOp (0, BArith BAadd , VarE (0, Var (Local , "k")),
Const (0, IntV 1));;


let env1 = { localvar = [("n", IntT ); ("k", IntT )]; globalvar = [];
returntp = VoidT ; funbind = []};;