#use "interpreter.ml"

let r = emptyenv Unbound
let execSem (e: exp) = sem(e, r)

(* [5;10;3] *)
(* [5;10] *)
let l1 = Eintlist(Cons(Eint(5),Cons(Eint(10), Cons(Eint(3),Nil))));;
let l2 = Eintlist(Cons(Eint(5),Cons(Eint(10),Nil)));;

(* (10 - (2*3)) + 2) -> 6 *)  
let testOP = OP("+",OP("-",Eint(10),OP("*",Eint(2),Eint(3))),Eint(2));;
execSem(testOP);;
(* 6 == 6 -> true *)
let testOPdue = OP("=", testOP, Eint(6));;
execSem(testOPdue);;
(* 6 <= 4 -> false *)
let testOPtre = OP("<=", testOP, Eint(4));;
execSem(testOPtre);;
(* not(true) and (true or false) -> false *)
let testBool = And(Not(Ebool(true)),Or(Ebool(true),Ebool(false)));;
execSem(testBool);;
(* l1 == l2 -> false *)
let testEq = Eq(l1,l2);;
execSem(testEq);;
(* l1 <= l2 -> true *)
let testMineq = Mineq(l2,l1);;
execSem(testMineq);;
(* l1 @ l2 -> [5;10;3;5;10] *)
let testAppend = Append(l1,l2);;
execSem(testAppend);;
(* l1.isEmpty -> false *)
let testIsempty = Isempty(l1);;
execSem(testIsempty);;
(* if (l1.isEmpty) then l1 @ l2 else l1 == l2 -> false *)
let testIf = IfThenElse(Isempty(l1),Append(l1,l2),Eq(l1,l2));;
execSem(testIf);;
(* let x = 3 in x + 2 -> 5 *)
let testLet = Let("x",Eint(3),OP("+",Den("x"),Eint(2)));;
execSem(testLet);;
(* f x = x + 3 *) 
let f = Fun("x", OP("+", Den("x"), Eint(3)));; 
(* f 5 -> 8 *)
let testFun = Appl(f, Eint(5));;
execSem(testFun);;
(* map f l1 -> [8;13;6] *)
let testMap = Map(f, l1);;
execSem(testMap);;


