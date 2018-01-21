(SINTASSI)
type ide = Ide of string;;

Ide "f";;

type exp =
Val of ide
| Eint of int
| Echar of char
| True
| False
| Empty
| Sum of exp * exp
| Diff of exp * exp
| Times of exp * exp
| And of exp * exp
| Or of exp * exp
| Not of exp
| Eq of exp * exp
| Less of exp * exp
| Cons of exp * exp
| Head of exp
| Tail of exp
| Fst of exp
| Snd of exp
| Epair of exp * exp
| Ifthenelse of exp * exp * exp
| Let of ide * exp * exp
| Fun of ide * exp
| Appl of exp * exp
| Rec of ide * exp;;

type env = ide -> eval;;

    type eval =
Undefined
| Int of int
| Bool of bool
| Char of char
| List of eval list
| Pair of eval * eval
| Closure of exp * env;;
  

type etype =
TBool
| TInt
| Tchar
| TVar of string
| TPair of etype * etype
| TList of etype list
| TFun of etype * etype;;

let nextsym = ref (-1);;
let newvar = fun () -> nextsym := !nextsym + 1;
TVar ("?T" ^ string_of_int (!nextsym));;

exception TypeExc of string;;

let newtypenv = [(Ide "",Tchar)];;

let rec applytypenv a i = match (a,i) with
  (Ide s,TBool)::tl,(Ide s1) -> if s1 = s then TBool else applytypenv tl i
| (Ide s,Tchar)::tl,(Ide s1) -> if s1 = s then Tchar else applytypenv tl i
| (Ide s,TInt)::tl,(Ide s1) ->  if s1 = s then TInt else applytypenv tl i
| (Ide s,TVar st)::tl,(Ide s1) -> if s1 = s then TVar st else applytypenv tl i
| (Ide s,TPair(e1,e2))::tl,(Ide s1) -> if s1 = s then TPair(e1,e2) else applytypenv tl i
| (Ide s,TList l)::tl,(Ide s1) -> if s1 = s then TList l else applytypenv tl i
| (Ide s,TFun(x,t))::tl,(Ide s1) -> if s1 = s then TFun(x,t) else applytypenv tl i
| [],_ -> failwith "ambiente tipi vuoto";;

let bindtyp env id tipo = match(id,tipo) with
   (Ide s,e) when e!=tipo-> (id,tipo)::env
| (Ide s,TBool)-> (id,TBool)::env
| (Ide s,Tchar)-> (id,Tchar)::env
| (Ide s,TInt)-> (id,TInt)::env
| (Ide s,TVar st)-> (id,TVar st)::env
| (Ide s,TPair(e1,e2))-> (id,TPair(e1,e2))::env
| (Ide s,TList l)-> (id,TList l)::env
| (Ide s,TFun(x,t))-> (id,TFun(x,t))::env;;

let env = newtypenv;;

let rec typeinf e = match e with
|Val(i)-> (match i with
    Ide s-> (applytypenv env i)
  |_->raise (TypeExc ("Not a ide")))
|Eint(e) -> TInt
|Echar(e) -> Tchar
|True -> TBool
|False -> TBool
|Sum(e1,e2)
|Diff(e1,e2)
|Times(e1,e2) -> if (typeinf e1) = TInt then
    if typeinf e2 = TInt then
      TInt else
      raise (TypeExc ("Not a TInt")) else
    raise (TypeExc ("Not a TInt"))
|And(e1,e2)
|Or(e1,e2) -> if typeinf e1 = TBool then
    if typeinf e2 = TBool then
      TBool else
      raise (TypeExc ("Not a TBool")) else
    raise (TypeExc ("Not a TBool"))
|Not(e) -> if typeinf e = TBool then TBool else raise(TypeExc ("Not a TBool"))
|Eq(e1,e2) -> if typeinf e1 = typeinf e2 then TBool else raise (TypeExc ("Different Types"))
|Less(e1,e2) -> if typeinf e1 = TInt then
    if typeinf e2 = TInt then
      TBool else
      raise (TypeExc ("Not a TInt")) else
    raise (TypeExc ("Not a TInt"))
|Cons(e1,e2) -> let tipo = typeinf e1 in
  if (typeinf e2 = TList (tipo::[])) || (e2 = Empty) then
    TList (tipo::[]) else
    raise (TypeExc ("Not compatible"))
|Empty -> TList (newvar()::[])
|Head(e) -> if typeinf e = TList ((typeinf e)::[]) then typeinf e else
  raise (TypeExc ("Not a TList"))
|Tail(e) -> if typeinf e = TList ((typeinf e)::[]) then TList((typeinf e)::[]) else
  raise (TypeExc ("Not a TList"))
|Fst(e)-> (match (typeinf e) with
   TPair(ex1,ex2)-> ex1
  | _ -> raise (TypeExc ("Not a TPair")))
|Snd(e) -> (match (typeinf e) with
   TPair(ex1,ex2)->  ex2
  | _ -> raise (TypeExc ("Not a TPair")))
|Epair(e1,e2) -> TPair(typeinf e1, typeinf e)
|Ifthenelse(t,e1,e2) -> if typeinf t = TBool then
    if typeinf e1 = typeinf e2 then
      typeinf e1 else
      raise (TypeExc ("Different types")) else
    raise (TypeExc ("Not a TBool"))
|Fun(x,t)-> let a = newvar() in
  bindtyp env x a;
    (match (x,typeinf t) with
    (Ide s,e) -> TFun(a,e));;
  
typeinf e1;;

let e = Fun(Ide "x",Ifthenelse(Eq(Val(Ide "x"),Empty),True,False));;
let e1=Fun(Ide "x",Fun(Ide "y",Val(Ide "x")));;
let e2=Eq(Eq(Cons(Eint 1, Cons(Eint 2, Empty)),Cons(Eint 1, Cons(Eint 2, Empty))),False);;
let e3 =Cons(Eint 1, Cons(Eint 2, Empty));;
let e4=Cons(Cons(Eint 1,Empty),Empty);;
let e5=Let(Ide "f", Fun(Ide "x",Fun(Ide "y",Val(Ide "x"))),
	   Appl(Appl(Val(Ide "f"),Eint 2),Eint 1));;
let e6 = Cons(Empty,Empty);;
let e7 = Fun(Ide "x", Ifthenelse(Eq(Val(Ide "x"),Empty),True,False));;
let e8 = Epair(Fun(Ide "x", Ifthenelse(Eq(Val(Ide "x"),Empty),True,False)),
      Cons(Cons(Eint 1,Empty),Empty));
let e9 = Val(Ide "x");;
