type variable = string
type symbol = string
type term = Variable of variable | Num of int | Constant of symbol | Atom of symbol | Node of symbol * (term list)
type atom = Atomic_formula of symbol * (term list)
type head = Head of atom
type body = Body of atom list
type clause = Fact of head | Query of body | Rule of head * body
type program = clause list

let rec string_of_term = function
  | Variable v -> "VARIABLE { " ^ v ^ " } "
  | Num n -> "INTEGER { " ^ string_of_int n ^ " } "
  | Constant c -> "CONSTANT { " ^ c ^ " } "
  | Atom at -> "ATOM { " ^ at ^ " } "
  | Node (s, []) -> "TERM { " ^ s ^ " }"
  | Node (s, args) -> 
    let args_str = String.concat ", " (List.map string_of_term args) in
    "TERM { " ^ s ^ "(" ^ args_str ^ ")" ^ " }"

let string_of_atom (Atomic_formula (s, args)) =
  let args_str = String.concat ", " (List.map string_of_term args) in
  if args = [] then
    "AT_FORMULA { " ^ s ^ " } "
  else
    "AT_FORMULA { " ^ s ^ "(" ^ args_str ^ ")" ^ " } "

let rec string_of_head = function
  | Head atom -> "HEAD { " ^ string_of_atom atom ^ " }"

let rec string_of_body = function
  | Body atoms -> "BODY { " ^ String.concat ", " (List.map string_of_atom atoms) ^ " }"

let rec string_of_clause = function
  | Query body -> "QUERY { " ^ string_of_body body ^ "}"
  | Fact head -> "FACT { " ^ string_of_head head ^ " }"
  | Rule (head, body) -> "RULE { " ^ string_of_head head ^ " => " ^ string_of_body body ^ " }"

let rec string_of_program = function
  | [] -> ""
  | clause :: clauses -> string_of_clause clause ^ "\n" ^ string_of_program clauses

