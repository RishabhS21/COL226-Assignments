type symbol = string * int;;
type signature = symbol list;;

type tree = V of string | C of { node: symbol; children: tree list };;

let check_sig (s : signature) : bool =
  let rec check_unique_symbols seen_symbols = function
    | [] -> true
    | (sym, _) :: tl ->
        if List.mem sym seen_symbols then false
        else check_unique_symbols (sym :: seen_symbols) tl
  in
  let rec check_non_negative_arity = function
    | [] -> true
    | (_, arity) :: tl -> arity >= 0 && check_non_negative_arity tl
  in
  check_unique_symbols [] s && check_non_negative_arity s

let rec wftree tree signature =
  let rec check_tree = function
    | V _ -> true
    | C { node = (name, arity); children } ->
      try
        let expected_arity = List.assoc name signature in
        if arity <> expected_arity || List.length children <> arity then false
        else List.for_all check_tree children
      with Not_found -> false
  in
  check_tree tree

let rec ht = function
  | V _ -> 0
  | C { children } -> 1 + List.fold_left (fun acc child -> max acc (ht child)) 0 children

let rec size = function
  | V _ -> 1
  | C { children } -> 1 + List.fold_left (fun acc child -> acc + size child) 0 children

let rec print_string_list = function
  | [] -> ()
  | [x] -> print_string x
  | x::xs -> print_string (x ^ ", "); print_string_list xs

let rec vars = function
  | V v -> [v]
  | C { children } -> List.flatten (List.map vars children)

let rec print_tree t =
  let rec string_of_tree t =
    match t with
    | V v -> v
    | C {node = (s, _); children = []} -> s
    | C {node = (s, _); children = c} ->
        s ^ "(" ^ String.concat ", " (List.map string_of_tree c) ^ ")"
  in
  print_endline (string_of_tree t)

let rec mirror = function
  | V v -> V v
  | C { node; children } -> C { node; children = List.rev (List.map mirror children) }

type substitution = (string * tree) list;;

let subst sigma t =
  let rec subst' = function
    | V v -> (match List.assoc_opt v sigma with Some t' -> t' | None -> V v)
    | C { node; children } -> C { node; children = List.map subst' children }
  in
  subst' t

let compose_subst s1 s2 =
  let composed_subst = List.map (fun (v, t) -> (v, subst s2 t)) s1 in
  composed_subst @ (List.filter (fun (v, _) -> not (List.mem_assoc v composed_subst)) s2)

let rec string_of_tree t =
  match t with
  | V x -> x
  | C { node = (s, _); children = ch } ->
      s ^ "(" ^ String.concat ", " (List.map string_of_tree ch) ^ ")"

let print_subst subst =
  let string_of_subst s =
    List.map (fun (x, t) -> x ^ " -> " ^ string_of_tree t) subst
    |> String.concat ", "
  in
  print_endline (string_of_subst subst)

exception NOT_UNIFIABLE;;

let rec present v t = match t with 
  | [] -> false
  | V vari :: tail -> if v = vari then
                        true
                      else
                         present v tail;
  | C {children} :: tail ->( present v children ) || (present v tail) ;;

let rec mgu t1 t2 =
  let rec unify_subtrees s t u = match t, u with
    | [], [] -> s
    | t1 :: rest1, t2 :: rest2 -> unify_subtrees (compose_subst s (mgu (subst s t1) (subst s t2))) rest1 rest2;
    | _ -> raise NOT_UNIFIABLE
  in
  match t1, t2 with
  | V v1, V v2 when v1 = v2 -> [];
  | V v1, V v2 when not(v1=v2) -> [(v1,t2)];
  | V v1, C {children} when not(present v1 children) -> [(v1, t2)];
  | V v1, C {children} when (present v1 children) -> raise NOT_UNIFIABLE;
  | C {children}, V v2 when not(present v2 children) -> [(v2, t1)];
  | C {children}, V v2 when (present v2 children) -> raise NOT_UNIFIABLE;
  | C { node = (n1, _); children = c1 }, C { node = (n2, _); children = c2 } when n1 = n2 ->
    unify_subtrees [] c1 c2;
  | _ -> raise NOT_UNIFIABLE

(*
--------------- checking base functions -----------------
let signature = [("f", 2); ("g", 1); ("h", 2)]
let tree1 = C { node = ("f", 2); children = [V "x"; C { node = ("g", 1); children = [V "y"] }] }
let children = [C { node = ("f", 2); children = [C { node = ("g", 1); children = [V "z"; C { node = ("g", 1); children = [V "y"] }] }; V "p"] }]
let substitution = [("x", V "a"); ("y", C { node = ("h", 2); children = [V "b"; V "c"] })]
let s1 = [("X", V "Y"); ("Y", V "Z")]
let s2 = [("Y", V "a"); ("Z", V "b")]

---------------- test cases for mgu functions -----------
(* t1-t2, mgu : x -> g(z) *)
let t1 = C { node = ("f", 2); children = [V "x"; V "y"] }
let t2 = C { node = ("f", 2); children = [C { node = ("g", 1); children = [V "z"] }; V "y"] }

(* t3-t4, mgu : x -> h(b, b), a -> b *)
let t3 = C { node = ("f", 2); children = [V "x"; V "x"] }
let t4 = C { node = ("f", 2); children = [C { node = ("h", 2); children = [V "a"; V "b"] }; C { node = ("h", 2); children = [V "b"; V "a"] }] }

(* t5-t6, mgu : X -> Z, Y -> g(Z) *)
let t5 = C { node = ("f", 2); children = [V "X"; V "Y"] }
let t6 = C { node = ("f", 2); children = [V "Z"; C { node = ("g", 1); children = [V "Z"] }] }

(* t7-t8, mgu : X -> a() *)
let t7 = V "X"
let t8 = C { node = ("a", 0); children = [] }

(* t9 - t10, mgu : NOT_UNIFIABLE *)
let t9 = V "X"
let t10 = C {node = ("f", 1); children = [V "X"]}

(* t11-t12, mgu : id_sub([]) :- nothing to print *)
let t11 = C {node = ("g",1); children = [V "Z"]}
let t12 = C {node = ("g",1); children = [V "Z"]}

(* t13-t14, mgu : X -> g(Y), Z -> g(Y) *)
let t13 = C { node = ("f", 2); children = [V "X"; C { node = ("g", 1); children = [V "Y"] }] }
let t14 = C { node = ("f", 2); children = [C { node = ("g", 1); children = [V "Y"] }; V "Z"] }

(* t15-t16, mgu : X -> g(h()), Z -> g(Y) *)
let t15 = C { node = ("f", 2); children = [V "X"; C { node = ("g", 1); children = [V "Y"] }] }
let t16 = C { node = ("f", 2); children = [C { node = ("g", 1); children = [C { node = ("h", 0); children = [] }] }; V "Z"] }

(* t17-t18, NOT_UNIFIABLE *)
let t17 = C { node = ("f", 1); children = [V "X"] }
let t18 = C { node = ("f", 2); children = [C { node = ("g", 1); children = [C { node = ("h", 0); children = [] }] }; V "Z"] }
*)

(*
------ helpers to check test cases ------
let () =
  print_endline (string_of_bool (present "y" [tree1]));;
  print_endline "Checking signature validity:";
  print_endline (string_of_bool (check_sig signature));
  print_endline "";

  print_endline "Checking well-formed tree:";
  print_endline (string_of_bool (wftree tree1 signature));
  print_endline "";

  print_endline "Height of tree:";
  print_int (ht tree1);
  print_endline "";

  print_endline "Size of tree:";
  print_int (size tree1);
  print_endline "";

  print_endline "Variables in tree:";
  print_string_list (vars tree1);
  print_endline "";

  print_endline "Mirror image of tree:";
  print_tree (mirror tree1);
  print_endline "";
  
  print_endline "Composition of two substitutions:";
  print_subst (compose_subst s1 s2); (* should print the composition of s1 and s2 *)
  print_endline "";

  print_endline "Applying substitution to tree:";
  print_tree (subst substitution tree1);
  print_endline "";

  print_endline "Most general unifier:";
  begin
    try
      print_subst (mgu t17 t18)
    with
      NOT_UNIFIABLE -> print_endline "NOT_UNIFIABLE"
  end;
  print_endline "";

*)