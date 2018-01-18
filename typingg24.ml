
  (**SINTASSI**)
  
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
| Rec of ide * exp

and
      
ide = Ide of string;;

    type eval =
Undefined
| Int of int
| Bool of bool
| Char of char
| List of eval list
| Pair of eval * eval
| Closure of exp * env

    and
	  env = ide -> eval;;

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

let rec typeinf e ide = match e with
|Val(i)-> TVar (ide i)
|Eint(e) -> TInt
|Echar(e) -> Tchar
|True -> TBool
|False -> TBool
|Empty -> TList (newvar a)/*************+/
|Sum(e1,e2)
|Diff(e1,e2)
|Times(e1,e2) -> if typeinf e1 = TInt then
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
|Cons(e1,e2) -> let tipo = typeinf e1 in if typeinf e2 = TList(tipo) then
    TList(tipo) else
    raise (TypeExc ("Not compatible"))
|Head(e) -> if typeinf e = TList then typeinf (List.hd e) else
  raise (TypeExc ("Not a TList"))
|Tail(e) -> if typeinf e = TList then TList(typeinf e) else
  raise (TypeExc ("Not a TList"))
|Fst(e) -> match typeinf e with
  | Tpair(ex1,ex2)-> ex1
  | _ -> raise (TypeExc ("Not a TPair"))
|Snd(e) -> match typeinf e with
  | Tpair(ex1,ex2)-> ex2
  | _ -> raise (TypeExc ("Not a TPair"))
|Epair(e1,e2) -> TPair(typeinf e1, typeinf e)
|Ifthenelse(t,e1,e2) -> if typeinf t = TBool then
    if typeinf e1 = typeinf e2 then
      typeinf e1 else
      raise (Typecheck ("Different types")) else
    raise (Typecheck ("Not a TBool"));
