type variable = string

type constant = string

type function_symbol = string

type term =
  | Variable of variable
  | Constant of constant
  | Function of function_symbol * term list

type atomic_formula = {
  predicate_symbol: function_symbol;
  terms: term list;
}

type clause =
  | Fact of atomic_formula
  | Rule of atomic_formula * atomic_formula list

type program = clause list

type goal = atomic_formula list

let rec string_of_term = function
  | Variable v -> v
  | Constant c -> c
  | Function (f, args) -> f ^ "(" ^ String.concat ", " (List.map string_of_term args) ^ ")"
 
let rec print_atomiclist ls = match ls with 
  | [] -> Printf.printf " \n"
  | hd::tl -> Printf.printf "%s\n"
                (hd.predicate_symbol ^ " " ^ String.concat " " (List.map (fun (t) -> (string_of_term t)) hd.terms));
      print_atomiclist tl
let rec print_program p = match p with 
  | [] -> Printf.printf "\n"
  | Rule(h, b)::tl -> print_atomiclist (h::b);
      print_program tl
  | Fact(h)::tl -> print_atomiclist [h];
      print_program tl
        
let rec substitute_term subst = function
  | Variable v -> (try List.assoc v subst with Not_found -> Variable v)
  | Constant c -> Constant c
  | Function (f, args) -> Function (f, List.map (substitute_term subst) args)

let substitute_formula subst { predicate_symbol; terms } =
  { predicate_symbol; terms = List.map (substitute_term subst) terms }

let rec unify_term t1 t2 subst =
  match t1, t2 with
  | Variable v1, Variable v2 when v1 = v2 -> Some subst
  | Variable v, t | t, Variable v ->
      if List.mem_assoc v subst then
        unify_term (List.assoc v subst) t subst
      else
        Some ((v, t) :: subst)
  | Constant c1, Constant c2 when c1 = c2 -> Some subst
  | Function (f1, args1), Function (f2, args2) when f1 = f2 ->
      unify_terms args1 args2 subst
  | _, _ -> None

and unify_terms ts1 ts2 subst =
  match ts1, ts2 with
  | [], [] -> Some subst
  | t1 :: rest1, t2 :: rest2 ->
      (match unify_term t1 t2 subst with
       | Some subst' -> unify_terms rest1 rest2 subst'
       | None -> None)
  | _, _ -> None

let unify_atom atom1 atom2 =
  unify_terms atom1.terms atom2.terms []

let rec resolve_goal goal program initial_program =
  match goal with
  | [] -> Some []
  | g :: rest_goal -> 
      (match resolve_atom g program initial_program with
       | Some subst -> 
           (match resolve_goal (List.map (substitute_formula subst) rest_goal) initial_program initial_program with
            | Some rest_subst -> Some (subst @ rest_subst)
            | None -> None)
       | None -> None)

and resolve_atom atom program initial_program = 
  match program with
  | [] -> None
  | Fact f :: rest_program ->
      begin 
        if f.predicate_symbol = atom.predicate_symbol then
          match unify_atom f atom with
          | Some subst -> Some subst
          | None -> resolve_atom atom rest_program initial_program
        else
          resolve_atom atom rest_program initial_program
      end

  | Rule (head, body) :: rest_program when head.predicate_symbol = atom.predicate_symbol ->
      (match unify_atom head atom with
       | Some subst -> 
           let substituted_body = (List.map (substitute_formula subst) body) in 
           (match resolve_goal substituted_body initial_program initial_program with
            | Some body_subst -> 
                Some body_subst 
            | None -> 
                None)
       | None -> resolve_atom atom rest_program initial_program)
  | _ :: rest_program -> resolve_atom atom rest_program initial_program

  
let rec update_var v =
  v ^ "!!"
  
let rec update_term = function
  | Variable v -> Variable (update_var v)
  | Constant c -> Constant c
  | Function (f, args) -> Function (f, List.map update_term args)
  
let rec update_atom a =
  { a with terms = List.map update_term a.terms }
  
let rec update_clause = function
  | Fact f -> Fact (update_atom f)
  | Rule (head, body) -> Rule (update_atom head, List.map update_atom body)
  
let rec find_var at_list = match at_list with
  | [] -> []
  | hd::tl -> 
      let rec match_var terms acc = match terms with 
        | [] -> acc
        | Variable v :: tl -> v::(match_var tl acc)
        | _ :: tl -> match_var tl acc in
      (match_var (hd.terms) [])@(find_var tl)
                                
let run_program program goal =
  let updated_program = List.map update_clause program in
  match resolve_goal goal updated_program updated_program with
  | Some subst -> 
      let filtered_subst = List.filter (fun (v, _) -> List.mem v (find_var goal)) subst in
      Printf.printf "Goals are satisfiable %s\n"
        (String.concat ", " (List.map (fun (v, t) -> v ^ " = " ^ (string_of_term t)) filtered_subst))
  | None -> print_endline "Goals are not satisfiable"


(* Test *)
let program = [
  Fact { predicate_symbol = "parent"; terms = [Constant "john"; Constant "mary"] };
  Fact { predicate_symbol = "parent"; terms = [Constant "john"; Constant "bob"] };
  Rule ({ predicate_symbol = "ancestor"; terms = [Variable "X"; Variable "Y"] },
        [{ predicate_symbol = "parent"; terms = [Variable "X"; Variable "Y"] }]);
]
let goal = [{ predicate_symbol = "ancestor"; terms = [Constant "john"; Variable "Y"] }; { predicate_symbol = "parent"; terms = [Constant "john"; Variable "Y"] }] 
let goal2 = [{ predicate_symbol = "ancestor"; terms = [Constant "john"; Variable "Z"] }]
let goal3 = [{ predicate_symbol = "ancestor"; terms = [Constant "john"; Constant "mary"] }] 
let _ = run_program program goal 
    
let program = [
  Fact { predicate_symbol = "parent"; terms = [Constant "john"; Constant "mary"] };
  Fact { predicate_symbol = "parent"; terms = [Constant "john"; Constant "bob"] };
] 
let goal = [{ predicate_symbol = "parent"; terms = [Constant "john"; Constant "mary"] }]
let _ = run_program program goal 
    
let program = [
  Fact { predicate_symbol = "parent"; terms = [Constant "john"; Constant "mary"] };
  Fact { predicate_symbol = "parent"; terms = [Constant "john"; Constant "bob"] };
  Fact { predicate_symbol = "parent"; terms = [Constant "mary"; Constant "james"] };
  Rule ({ predicate_symbol = "grandparent"; terms = [Variable "X"; Variable "Z"] },
        [{ predicate_symbol = "parent"; terms = [Variable "X"; Variable "Y"] };
         { predicate_symbol = "parent"; terms = [Variable "Y"; Variable "Z"] }])
] 
let goal = [{ predicate_symbol = "grandparent"; terms = [Constant "john"; Constant "james"] }]
let goal2 = [{ predicate_symbol = "grandparent"; terms = [Constant "john"; Variable "X"] }]
let _ = run_program program goal  
    
let program = [
  Fact { predicate_symbol = "parent"; terms = [Constant "john"; Constant "mary"] };
  Fact { predicate_symbol = "parent"; terms = [Constant "mary"; Constant "alice"] }
] 
let goal = [{ predicate_symbol = "parent"; terms = [Variable "X"; Variable "Y"] }]
let _ = run_program program goal 
    
let program = [
  Fact { predicate_symbol = "parent"; terms = [Constant "john"; Constant "mary"] };
  Fact { predicate_symbol = "parent"; terms = [Constant "mary"; Constant "alice"] };
  Rule ({ predicate_symbol = "grandparent"; terms = [Variable "X"; Variable "Z"] },
        [{ predicate_symbol = "parent"; terms = [Variable "X"; Variable "Y"] };
         { predicate_symbol = "parent"; terms = [Variable "Y"; Variable "Z"] }])
] 
let goal = [{ predicate_symbol = "grandparent"; terms = [Constant "john"; Variable "P"] }]
           
let program = [
  Fact { predicate_symbol = "sibling"; terms = [Constant "alice"; Constant "bob"] };
  Fact { predicate_symbol = "sibling"; terms = [Constant "bob"; Constant "john"] };
  Rule ({ predicate_symbol = "cousin"; terms = [Variable "X"; Variable "Y"] },
        [{ predicate_symbol = "sibling"; terms = [Variable "X"; Variable "Z"] };
         { predicate_symbol = "sibling"; terms = [Variable "Z"; Variable "Y"] }]);
  Fact { predicate_symbol = "likes"; terms = [Constant "john"; Constant "apples"] };
  Fact { predicate_symbol = "likes"; terms = [Constant "mary"; Constant "oranges"] }
]


let goal = [{ predicate_symbol = "cousin"; terms = [Constant "alice"; Variable "X"] }; { predicate_symbol = "likes"; terms = [Variable "X"; Constant "apples"] }]

let program = [
  Fact { predicate_symbol = "owns"; terms = [Constant "alice"; Constant "book1"] };
  Fact { predicate_symbol = "owns"; terms = [Constant "bob"; Constant "book1"] };
  Rule ({ predicate_symbol = "friend"; terms = [Variable "X"; Variable "Y"] },
        [{ predicate_symbol = "owns"; terms = [Variable "X"; Variable "Z"] };
         { predicate_symbol = "owns"; terms = [Variable "Y"; Variable "Z"] }])
]

let goal = [{ predicate_symbol = "friend"; terms = [Variable "X"; Constant "bob"] }]
      
let _ = run_program program goal 
    
    