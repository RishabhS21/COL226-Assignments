(* Types for expressions *)

type exp = Int of int | Bool of bool | Var of string | Abs of string * exp | App of exp * exp 
         | Add of exp*exp | Sub of exp*exp | Mul of exp*exp | Div of exp*exp
         | And of exp*exp | Or of exp*exp | Not of exp
         | Gt of exp*exp | Lt of exp*exp | Eq of exp*exp | Ifthenelse of exp*exp*exp
         | Tuple of exp list | Projection of exp*exp
         | Case of exp*((int*string) list);;

(* Type of opcodes *)

type opcode =  INT of int | BOOL of bool | LOOKUP of string | MKCLOS of string*(opcode list) | APP | RET 
            | ADD | SUB | MUL | DIV
            | AND | OR | NOT 
            | GT | LT | EQ | IFTHENELSE
            | TUPLE of int | PROJECTION
            | CASE of (int*string) list;;


(* Defining secd components' types *)

type stack = answer list
and environment = (string*answer) list
and code = opcode list
and dump = (stack*environment*code) list
and answer = Numeral of int | Boolean of bool | Vclos of environment*string*code | Tuple_ of answer list | String of string
;;

(* Compile function : Return an opcode list for the expression *)

let rec compile e = match e with 
  | Int(i) -> [INT(i)] 
  | Bool(b) -> [BOOL(b)]
  | Var(x) -> [LOOKUP(x)]
  | Abs(x, e1) -> [MKCLOS(x, (compile e1) @ [RET])]
  | App(e1, e2) -> (compile e1) @ (compile e2) @ [APP] 
                                                 
  | Add(e1, e2) -> (compile e1) @ (compile e2) @ [ADD]
  | Sub(e1, e2) -> (compile e1) @ (compile e2) @ [SUB]
  | Mul(e1, e2) -> (compile e1) @ (compile e2) @ [MUL]
  | Div(e1, e2) -> (compile e1) @ (compile e2) @ [DIV]
                   
  | And(e1, e2) -> (compile e1) @ (compile e2) @ [AND]
  | Or(e1, e2) -> (compile e1) @ (compile e2) @ [OR]
  | Not(e1) -> (compile e1) @ [NOT]
                           
  | Gt(e1, e2) -> (compile e1) @ (compile e2) @ [GT]
  | Lt(e1, e2) -> (compile e1) @ (compile e2) @ [LT]
  | Eq(e1, e2) -> (compile e1) @ (compile e2) @ [EQ]
  | Ifthenelse(e1, e2, e3) -> (compile e1) @ (compile e2) @ (compile e3) @ [IFTHENELSE]
                                                 
  | Tuple(exp_list) -> let len = List.length exp_list in
      List.flatten (List.map compile exp_list) @ [TUPLE(len)]
  | Projection(e1, e2) -> (compile e1) @ (compile e2) @ [PROJECTION]
  | Case(e1, l) -> (compile e1) @ [CASE(l)];;


(* SECD machine implementation *)

let rec secd = function
  | (a::s, env, [], dmp) -> a
  | (s, env, INT(i)::cde, dmp) -> secd (Numeral(i)::s, env, cde, dmp)
  | (s, env, BOOL(b)::cde, dmp) -> secd (Boolean(b)::s, env, cde, dmp)
  | (s, env, LOOKUP(x)::cde, dmp) -> 
      let rec lookup x table = match table with
          [] -> failwith "Variable not found"
        |(var, value)::t' -> if var = x then value else lookup x t' in
      secd ((lookup x env)::s, env, cde, dmp)
  | (s, env, MKCLOS(x, cde')::cde, dmp) -> secd (Vclos(env, x, cde')::s, env, cde, dmp)
  | (a::Vclos(env', x, cde')::s, env, APP::cde, dmp) -> secd ([], (x, a)::env', cde', (s,env, cde)::dmp)
  | (a::s, env, RET::cde, (s',env', cde')::dmp) -> secd(a::s', env', cde', dmp)
                                                     
  | (Numeral(a1)::Numeral(a2)::s, env, ADD::cde, dmp) -> secd (Numeral(a1+a2)::s, env, cde, dmp)
  | (Numeral(a1)::Numeral(a2)::s, env, SUB::cde, dmp) -> secd (Numeral(a2-a1)::s, env, cde, dmp)
  | (Numeral(a1)::Numeral(a2)::s, env, MUL::cde, dmp) -> secd (Numeral(a1*a2)::s, env, cde, dmp)
  | (Numeral(a1)::Numeral(a2)::s, env, DIV::cde, dmp) -> secd (Numeral(a2/a1)::s, env, cde, dmp)
                                               
  | (Boolean(b1)::Boolean(b2)::s, env, AND::cde, dmp) -> 
      let b = b1 && b2 in 
      secd (Boolean(b)::s, env, cde, dmp)
  | (Boolean(b1)::Boolean(b2)::s, env, OR::cde, dmp) -> 
      let b = b1 || b2 in 
      secd (Boolean(b)::s, env, cde, dmp)
  | (Boolean(b1)::s, env, NOT::cde, dmp) -> 
      let b = not b1 in 
      secd (Boolean(b)::s, env, cde, dmp)

  | (Numeral(i1)::Numeral(i2)::s, env, GT::cde, dmp) ->
      let b = i2 > i1 in
      secd (Boolean(b)::s, env, cde, dmp)
  | (Numeral(i1)::Numeral(i2)::s, env, LT::cde, dmp) ->
      let b = i2 < i1 in
      secd (Boolean(b)::s, env, cde, dmp)
  | (Numeral(i1)::Numeral(i2)::s, env, EQ::cde, dmp) ->
      let b = i2 == i1 in
      secd (Boolean(b)::s, env, cde, dmp)
  | (a1::a2::Boolean(b)::s, env, IFTHENELSE::cde, dmp) ->
      let a = if b then a2 else a1 in
      secd (a::s, env, cde, dmp)
                                               
  | (s, env, TUPLE(n)::cde, dmp) ->
      let rec take n lst = match n, lst with
        | 0, _ -> []
        | n, [] -> failwith "Error while taking!!"
        | n, hd::tl -> take (n-1) tl @ [hd] in

      let rec drop n lst = match n, lst with
        | 0, lst -> lst
        | n, [] -> failwith "Error while dropping!!"
        | n, hd::tl -> drop (n-1) tl in 

      let vals = take n s in
      let s' = drop n s in
      secd (Tuple_(vals)::s', env, cde, dmp)
        
  | (Numeral(i)::Tuple_(a_list)::s, env, PROJECTION::cde, dmp) -> 
      let rec find n lst = match n, lst with
        | _, [] -> failwith "Length does not match!!"
        | 0, hd::tl -> hd
        | n, hd::tl -> find (n-1) tl in
      let a = find i a_list in
      secd (a::s, env, cde, dmp)
        
  | (Numeral(i)::s, env, CASE(l)::cde, dmp) ->
      let rec match_case i lst = match lst with
        | [] -> failwith "No matching case found"
        | (n, str) :: tl -> if n = i then str else match_case i tl in
      
      let str = match_case i l in
      secd (String(str)::s, env, cde, dmp)
        
  | _ -> failwith "Invalid operation found";;

let call_secd env oplist = secd([], env, oplist, []);;

(* Some Exhaustive test-cases *)

let example_env = [("b", Numeral 3); ("d", Numeral 10); ("x", Numeral 6); ("y", Numeral 5); ("z", Boolean true)];;

let t1 = App(Abs("b", Add(Var "x", Int 7)), Add(Var "b", Int 8));; 
call_secd example_env (compile t1);;

let t2 = Add(Var "b", Int 9);;
call_secd example_env (compile t2);; 

let t3 = (App(Abs("x",App(Abs("d",Mul(Var("d"),Int(2))),Int(2))),Int(2)));;
call_secd example_env (compile t3)

let t4 = (App(Abs("x",Add(Var "x", App(Abs("d",Mul(Var("d"),Int(2))),Int(2)))),Int(2)));;
call_secd example_env (compile t4)

let t5 = Var "z";;
call_secd example_env (compile t5);; 

let t6 = App(Abs("z", And(Var "z", Bool(false))), Bool true);;
call_secd example_env (compile t6);; 

let t7 = Tuple([Mul(Var "x", Int 7); Lt(Int(121), Int(25)); Eq(Int(5), Div(Var("y"), Int 1))]);;
call_secd example_env (compile t7);; 

let t8 = Projection(Tuple([Add(Var "x", Int 7);Int(12);Int(3)]), Int(2));;
call_secd example_env (compile t8);; 

let t9 = (Ifthenelse(Gt(Int(1),Int(2)),Add(Int(1),Int(3)),Sub(Int(1),Int(3))));;
call_secd example_env (compile t9);; 

let t10 = Case(Int 3, [(2, "Two"); (3, "Three"); (5, "Five")]);;
call_secd example_env (compile t10);; 
      