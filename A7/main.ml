(* Defining expression types *)
type exp = Int of int | Bool of bool | Var of string | Abs of string * exp | App of exp * exp 
         | Add of exp * exp | Sub of exp * exp | Mul of exp*exp | Div of exp*exp
         | And of exp*exp | Or of exp*exp | Not of exp
         | Gt of exp*exp | Lt of exp*exp | Eq of exp*exp | Ifthenelse of exp*exp*exp
         | Tuple of exp list | Projection of exp*exp
         | Case of exp*((int*string) list)
;; 

(* Defining krivine components' types *)

type closSTACK = clos list
and clos = Clos of exp * envCLOS | TupleCLOS of clos list | StringCLOS of string * envCLOS
and envCLOS = (string * clos) list
and answer = Numeral of int | Boolean of bool | Tuple_ of answer list | String of string
;;

(* Krivine machine implementation *)

let rec krivine c s = match c with
  | Clos(Int i, env) -> Clos(Int i, env)
  | Clos(Bool i, env) -> Clos(Bool i, env)
  | Clos(Var v, env) -> 
      let rec lookup v env = match env with
        | [] -> failwith "Variable not found!!"
        | (x, cl)::env' -> if x = v then (match cl with 
            | Clos(Abs(x, e), env) -> Clos(Abs(x, e), (v, cl)::env) 
            | _ -> cl) else lookup v env' in
  
      krivine (lookup v env) s
  | Clos(Abs(x, e), env) -> (match s with 
      | cl::s' -> krivine (Clos(e, (x, cl)::env)) s'
      | [] -> failwith "Abstraction error!!"
    )
  | Clos(App(e1, e2), env) -> krivine (Clos(e1, env)) (Clos(e2, env)::s)
  | Clos(Add(e1, e2), env) ->
      let add (c1, c2) = match (c1, c2) with 
        | Clos(Int i1, env1), Clos(Int i2, env2) -> Clos(Int(i1+i2), [])
        | _ -> failwith "Addition error!!" in
      krivine (add ((krivine (Clos(e1, env)) []), (krivine (Clos(e2, env)) []))) s
        
  | Clos(Sub(e1, e2), env) ->
      let sub(c1, c2) = match (c1, c2) with 
        | Clos(Int i1, env1), Clos(Int i2, env2) -> Clos(Int(i1-i2), [])
        | _ -> failwith "Substraction error!!" in
      krivine (sub ((krivine (Clos(e1, env)) []), (krivine (Clos(e2, env)) []))) s
      
  | Clos(Mul(e1, e2), env) ->
      let mul (c1, c2) = match (c1, c2) with 
        | Clos(Int i1, env1), Clos(Int i2, env2) -> Clos(Int(i1*i2), []) 
        | _ -> failwith "MUltiplication error!!" in
      krivine (mul ((krivine (Clos(e1, env)) []), (krivine (Clos(e2, env)) []))) s
        
  | Clos(Div(e1, e2), env) ->
      let div(c1, c2) = match (c1, c2) with 
        | Clos(Int i1, env1), Clos(Int i2, env2) -> Clos(Int(i1/i2), []) 
        | _ -> failwith "Division error!!" in
      krivine (div ((krivine (Clos(e1, env)) []), (krivine (Clos(e2, env)) []))) s
        
  | Clos(And(e1, e2), env) ->
      let andb(c1, c2) = match (c1, c2) with 
        | Clos(Bool b1, env1), Clos(Bool b2, env2) -> Clos(Bool(b1 && b2), []) 
        | _ -> failwith "Boolean Operation 'And' failed!!" in
      krivine (andb ((krivine (Clos(e1, env)) []), (krivine (Clos(e2, env)) []))) s
        
  | Clos(Or(e1, e2), env) ->
      let orb(c1, c2) = match (c1, c2) with 
        | Clos(Bool b1, env1), Clos(Bool b2, env2) -> Clos(Bool(b1 || b2), []) 
        | _ -> failwith "Boolean Operation 'Or' failed!!" in
      krivine (orb ((krivine (Clos(e1, env)) []), (krivine (Clos(e2, env)) []))) s
        
  | Clos(Not(e1), env) ->
      let notb(c1) = match (c1) with 
        | Clos(Bool b1, env1) -> Clos(Bool(not b1), []) 
        | _ -> failwith "Boolean Operation 'Not' failed!!" in
      krivine (notb ((krivine (Clos(e1, env)) []))) s
        
  | Clos(Gt(e1, e2), env) ->
      let gtb (c1, c2) = match (c1, c2) with 
        | Clos(Int i1, env1), Clos(Int i2, env2) -> Clos(Bool(i1>i2), []) 
        | _ -> failwith "'Greater than' operation failed!!" in
      krivine (gtb ((krivine (Clos(e1, env)) []), (krivine (Clos(e2, env)) []))) s
        
  | Clos(Lt(e1, e2), env) ->
      let ltb (c1, c2) = match (c1, c2) with 
        | Clos(Int i1, env1), Clos(Int i2, env2) -> Clos(Bool(i1<i2), []) 
        | _ -> failwith "'Less than' operation failed!!" in
      krivine (ltb ((krivine (Clos(e1, env)) []), (krivine (Clos(e2, env)) []))) s
        
  | Clos(Eq(e1, e2), env) ->
      let eqb (c1, c2) = match (c1, c2) with 
        | Clos(Int i1, env1), Clos(Int i2, env2) -> Clos(Bool(i1 == i2), [])
        | _ -> failwith "'Equal' operation failed!!"  in 
      krivine (eqb ((krivine (Clos(e1, env)) []), (krivine (Clos(e2, env)) []))) s
        
  | Clos(Ifthenelse(e1, e2, e3), env) -> 
      (match krivine (Clos(e1, env)) [] with
       | Clos(Bool b1, env1) ->
           let e = if b1 then e2 else e3 in
           krivine (Clos(e, env1)) s
       | _ -> failwith "Ifthenelse failed!!")

  | Clos(Tuple(exp_list), env) ->
      let rec eval_tuple exp_list acc = match exp_list with
        | [] -> List.rev acc
        | e::exp_list' ->
            let result = krivine (Clos(e, env)) [] in
            eval_tuple exp_list' (result :: acc)
      in
      TupleCLOS((eval_tuple exp_list []))

  | Clos(Projection(e1, e2), env) ->
      let t, i = (krivine (Clos(e1, env)) []), (krivine (Clos(e2, env)) []) in
      (match (t, i) with
       | (TupleCLOS(ls), Clos(Int n, _)) -> 
           let rec find n ls = match (n, ls) with
             | _, [] -> failwith "length does not match!!"
             | 0, hd::tl -> hd
             | n, hd::tl -> find (n-1) tl in
           find n ls
       | _ -> failwith "Projection error!!")
      
  | Clos(Case(e, l), env) ->
      let i = (krivine(Clos(e, env)) []) in
      (match i with 
       | Clos(Int n , _) ->
           let rec match_case n l = match l with
               (m, st)::tl -> if n = m then StringCLOS(st, env) else match_case n tl
             | [] -> failwith "No match found!!" in
           match_case n l
       | _ -> failwith "Case error!!")
      
  | _ -> failwith "Invalid Expression!!"
               
;;


let rec krivinemachine p env = 
  match krivine (Clos(p, env)) [] with 
  | Clos (Int i, _) -> Numeral i
  | Clos (Bool b, _) -> Boolean b
  | StringCLOS(st, _) -> String st
  | TupleCLOS(ls) -> 
      let rec make_tuple ls acc = match ls with
        | [] -> List.rev acc
        | e::ls' -> (match e with
              Clos(Int i, _) -> make_tuple ls' ((Numeral i)::acc) 
            | Clos(Bool b, _) -> make_tuple ls' ((Boolean b)::acc)
            | StringCLOS(st, _) -> make_tuple ls' ((String st)::acc)
            | _ -> failwith "Invalid Closure Type!!" ) in
      Tuple_((make_tuple ls []))
  | _ -> failwith "Invalid Closure type!!";;

(* exhaustive test cases *)
let p = App(Abs("x", Add(Var "x", Int 0)), Int 3);;
let p = Mul(Int 6, Int 2);;
let p = And(Bool false, Bool false);; 
let p = Ifthenelse(Bool false, Add(Int 5, Int 81), Lt(Int 7, Int 9));;
let p = Tuple[Int 5; Int 6; Add(Int 7, Int 9); Gt(Int 7, Int 9)];;
let p = Projection(Tuple[Int 5; Int 6; Add(Int 7, Int 9); Gt(Int 7, Int 9)], Add(Int 2, Int(-1)));;
let p = Case(Int 3, [(2, "Two"); (3, "Three"); (5, "Five")]);;
let p = App(Abs("x", Not(Or(Bool false, Var "x"))), Bool false);;
let p = App (Abs ("x", App(Abs ("y", Add (Var "x", Var "y")), Int 7)), Int 5);;
let p = Ifthenelse(Gt(Int(1),Int(5)),App(Abs("x", Div(Var("x"),Int(2))),Int(2)),App(Abs("x", Mul(Var("x"),Add(Var("x"),Int(1)))),Int(2)));;
(* 
let p = And(Int 5, Bool true);; (* should give an error *)
 *)
let env = [];;
krivinemachine p env;;
let p1 = Div(Int(72),Var("y"));;
let p1 = App(Abs("x", Or(Eq(Int 6, Var "y"), Var "x")), Bool true);;
let env1 = [("y", Clos(Int 9, [])); ("b", Clos(Bool false, []))];;
krivinemachine p1 env1;;