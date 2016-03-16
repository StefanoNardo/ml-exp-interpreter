(* Environment *) 
type 't env = (string * 't) list	 
exception WrongBindlist 
let emptyenv(x) = [("",x)]
let rec applyenv(x,y) = match x with
	| [(_,e)] -> e
	| (i1,e1) :: x1 -> if y = i1 then e1 
		else applyenv (x1, y)
	| [] -> failwith ("Wrong environment")	
let bind(r, l, e) = (l, e) :: r

(* Abstract syntax *)

type ide = string


type intlist = Nil | Cons of exp * intlist 

and exp = 
	| Eint of int
	| Ebool of bool
	| Eintlist of intlist
	| Den of ide
	| OP of string * exp * exp 
	| And of exp * exp
	| Or of exp * exp
	| Not of exp
	| Eq of exp * exp
	| Mineq of exp * exp
	| Isempty of exp
	| Append of exp * exp
	| IfThenElse of exp * exp * exp
	| Let of ide * exp * exp
	| Fun of ide * exp
	| Appl of exp * exp
	| Map of exp * exp
	| Appllist of exp * int

and eval =
	| Int of int
	| Bool of bool
	| Intlist of intlist
	| Unbound
	| Funval of efun


and efun = exp * eval env


(* Operations on eval *)

let plus (x,y) = 
	(match (x,y) with 
	| (Int(u), Int(w)) -> Int(u+w)
	| _ -> failwith ("type error"))

let diff (x,y) =
	(match (x,y) with
	| (Int(u), Int(w)) -> Int(u-w)
	| _ -> failwith ("type error"))

let mult (x,y) =
	(match (x,y) with
	| (Int(u), Int(w)) -> Int(u*w)
	| _ -> failwith ("type error"))

let eq (x,y) =
	(match (x,y) with
	| (Int(u), Int(w)) -> Bool(u = w)
	| _ -> failwith ("type error"))

let mineq (x,y) = 
	(match (x,y) with
	| (Int(u), Int(w)) -> Bool(u <= w)
	| _ -> failwith ("type error"))

let et (x,y) =
	(match (x,y) with
	| (Bool(u), Bool(w)) -> Bool(u && w)
	| _ -> failwith ("type error"))

let opp (x,y) =
	(match (x,y) with
	| (Bool(u), Bool(w)) -> Bool(u || w)
	| _ -> failwith ("type error"))

let non x =
	(match x with 
	| Bool(u) -> Bool(not u)
	| _ -> failwith ("type error"))

let rec compare x y = 
	(match x, y with
	| Nil, Nil -> true
	| Nil, _
	| _, Nil -> false
	| Cons(Eint(h1), l1), Cons(Eint(h2), l2) -> h1 = h2 && compare l1 l2
	| _ -> failwith ("type error"))

let rec sublist x y = 
	(match x, y with
	| Nil, _ -> true
	| _, Nil -> false
	| Cons(Eint(h1), l1), Cons(Eint(h2), l2) -> h1 = h2 && sublist l1 l2
	| _ -> failwith ("type error"))

let isempty x = 
	(match x with
	| Nil -> Bool(true)
	| _ -> Bool(false))

let rec append x y = 
	(match x  with 
	| Nil -> y 
	| Cons(Eint(hd), l) -> Cons(Eint(hd), append l y)
	| _ -> failwith ("type error"))

(* Auxiliary function for map *)

let toeint n = 
	(match n with 
	| Int(n) -> Eint(n)
	| _ -> failwith ("type error"))

(* Interpreter's semantic *)

let rec sem ((e:exp), (r:eval env)) =
		match e with 
	| Eint(n) -> Int(n)
	| Ebool(b) -> Bool(b)
	| Eintlist(l) -> Intlist(l)
	| Den(i) -> applyenv(r,i)
    | OP(op,a,b) -> 
		(match op with
		| "+" -> plus(sem(a, r), sem(b, r))
		| "-" -> diff(sem(a, r), sem(b, r))
		| "*" -> mult(sem(a, r), sem(b, r))
		| "=" -> eq(sem(a, r), sem(b, r))
		| "<=" -> mineq(sem(a, r), sem(b, r)) 
		| _ -> failwith ("wrong operator"))
	| And(a,b) -> et(sem(a, r), sem(b, r))
	| Or(a,b) -> opp(sem(a, r), sem(b, r)) 
	| Not(b) -> non(sem(b, r))
	| Eq(a,b) -> (match (sem(a,r), sem(b, r)) with
		| (Intlist(u), Intlist(w)) -> Bool(compare u w)
		| _ -> failwith ("type error"))
	| Mineq(a,b) -> (match (sem(a,r), sem(b, r)) with
		| (Intlist(u), Intlist(w)) -> Bool(sublist u w)
		| _ -> failwith ("type error"))
	| Append(a,b) -> (match (sem(a, r), sem(b, r)) with
		| (Intlist(u), Intlist(w)) -> Intlist(append u w)
		| _ -> failwith ("type error"))
	| Isempty(l) -> (match sem(l, r) with 
		| Intlist(u) -> isempty u
		| _ -> failwith ("type error"))
	| IfThenElse(a,b,c) -> 
		let g = sem(a, r) in
			(if g = Bool(true) then sem(b, r) 
			else (if g = Bool(false) then sem(c, r)	
			else failwith ("nonboolean guard")))
	| Let(i,e1,e2) -> sem(e2, bind(r, i, sem(e1, r))) 
	| Fun(x,a) -> Funval(e, r)
	| Appl(e1,e2) -> 
		(match sem(e1, r) with 
		| Funval(Fun(x, a), r1) -> 
			sem(a, bind(r1, x, sem(e2, r)))
		| _ -> failwith ("no funct in apply"))
	| Map(e1,e2) ->
		(match sem(e1, r) with
		| Funval(Fun(x, a), r1) -> 
			(match sem(e2, r) with
			| Intlist(u) -> 
				Intlist((let rec map f e = 
					(match e with
					| Nil -> Nil
					| Cons(Eint(hd),tl) -> Cons(toeint(sem(Appl(f,Eint(hd)), r)), map f tl)
					| _ -> failwith ("type error"))
				in map e1 u))
			| _ -> failwith ("type error"))
		| _ -> failwith ("no funct in apply"))
	| _ -> failwith ("nonlegal expression for sem")


