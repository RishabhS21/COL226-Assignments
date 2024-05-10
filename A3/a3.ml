(* defining tokens *)
type token =
  | IDENTIFIER of string
  | KEYWORD of string
  | BOOLEAN of bool
  | INTEGER of int
  | ARITH_OP of string
  | COMP_OP of string
  | BOOL_OP of string
  | STRING_OP of string
  | PAREN of string
  | COMMA
  | ERROR of string


(* basic helper functions *)
let is_digit c = '0' <= c && c <= '9'
let is_alpha c = ('a' <= c && c <= 'z')
let is_uppercase c = ('A' <= c && c <= 'Z')
let is_alnum c = is_alpha c || is_uppercase c || is_digit c || c = '\'' || c = '_'

(* processing single token*)
let rec take_while predicate str acc =
  match str with
  | c :: cs when predicate c -> take_while predicate cs (c :: acc)
  | _ -> List.rev acc, str

(* main tokeniser function *)

let rec tokenize_aux = function
  | c :: cs when is_digit c ->
    let digits, rest = take_while is_digit (c :: cs) [] in
    let rest_head, rest_tail = match rest with
      | [] -> ' ', []
      | h :: tl -> h, tl in
    if rest_head = ' ' || rest_head = '\n' || not (is_alnum rest_head) then
      (INTEGER (int_of_string (String.of_seq (List.to_seq digits)))) :: tokenize_aux rest
    else
        let rec invalid_string acc = function
            | [] -> String.concat "" (List.rev acc)
            | ' ' :: _ -> String.concat "" (List.rev acc)
            | x :: xs -> invalid_string (String.make 1 x :: acc) xs
        in
        let rec split_at_space = function
            | ' ' :: rest -> rest
            | _ :: rest -> split_at_space rest
            | [] -> []
        in
        let updated_rest = split_at_space rest in
        let invalid_part = invalid_string [] (digits @ rest) in
        let error_msg = Printf.sprintf "Invalid token '%s', as an identifier" invalid_part in
        (ERROR error_msg) :: tokenize_aux updated_rest

  | c :: cs when is_alpha c || c = '_' ->
    let identifier, rest = take_while is_alnum (c :: cs) [] in
    let token = match String.of_seq (List.to_seq identifier) with
      | "if" | "then" | "else" | "tuple" | "first" | "second" -> KEYWORD (String.of_seq (List.to_seq identifier))
      | "not" | "or" | "and" -> BOOL_OP (String.of_seq (List.to_seq identifier))
      | "true" -> BOOLEAN true
      | "false" -> BOOLEAN false
      | id -> IDENTIFIER id
    in
    token :: tokenize_aux rest

  | c :: cs when is_uppercase c || c = '\'' ->
    let rec invalid_string acc = function
        | [] -> String.concat "" (List.rev acc)
        | ' ' :: _ -> String.concat "" (List.rev acc)
        | x :: xs -> invalid_string (String.make 1 x :: acc) xs
    in
    let rec split_at_space = function
        | ' ' :: rest -> rest
        | _ :: rest -> split_at_space rest
        | [] -> []
    in
    let rest = split_at_space cs in
    let invalid_part = invalid_string [] (c :: cs) in
    let error_msg = Printf.sprintf "Invalid token '%s', as an identifier" invalid_part in
    (ERROR error_msg) :: tokenize_aux rest

  | '+' :: cs -> ARITH_OP "+" :: tokenize_aux cs
  | '-' :: cs -> ARITH_OP "-" :: tokenize_aux cs
  | '*' :: cs -> ARITH_OP "*" :: tokenize_aux cs
  | '/' :: cs -> ARITH_OP "/" :: tokenize_aux cs
  | '>' :: '=' :: cs -> COMP_OP ">=" :: tokenize_aux cs
  | '>' :: cs -> COMP_OP ">" :: tokenize_aux cs
  | '<' :: '=' :: cs -> COMP_OP "<=" :: tokenize_aux cs
  | '<' :: cs -> COMP_OP "<" :: tokenize_aux cs
  | '=' :: '=' :: cs -> COMP_OP "==" :: tokenize_aux cs
  | '=' :: cs -> COMP_OP "=" :: tokenize_aux cs
  | '&' :: '&' :: cs -> BOOL_OP "&&" :: tokenize_aux cs
  | '|' :: '|' :: cs -> BOOL_OP "||" :: tokenize_aux cs
  | '!' :: cs -> BOOL_OP "!" :: tokenize_aux cs
  | '^' :: cs -> STRING_OP "^" :: tokenize_aux cs
  | ',' :: cs -> COMMA :: tokenize_aux cs
  | '(' :: cs -> PAREN "(" :: tokenize_aux cs
  | ')' :: cs -> PAREN ")" :: tokenize_aux cs
  | ' ' :: cs | '\n' :: cs -> tokenize_aux cs
  | c :: cs ->
    let error_msg = Printf.sprintf "Unexpected character '%c'" c in
    (ERROR error_msg) :: tokenize_aux cs

  | [] -> []

let tokenize str = tokenize_aux (List.of_seq (String.to_seq str))

(* Test cases *)
let () =
  let test_cases = [
    "2376gn wgkeu";
    "1, 2, 3";
    "_5#6";
    "if then a#b Gejw 126b else _variable prime' iFthen _ a08392b 0123 + - * / > >= < <= == && , ()";
    "ifthen x y z _var _var' x123 y_123 _x123";
    "Xyz hJIHii";
    "var12";
    "X123";
    "1*(3)";
    "if then else";
    "tuple fst snd";
    "true false";
    "1 + 2 * 3";
    "x = 10";
    "x > y";
    "x < y";
    "x >= y";
    "x <= y";
    "x == y";
    "x && y";
    "x || y";
    "not x";
    "x123, y_123, _x123";
    "x123  , y_123 , _x123";
    "x + y";
    "x - y";
    "x * y";
    "x / y";
    "(x, y)";
    "1, 2";
    "(x, y), z";
    "1, 2, 3";
    "\"string\"";
    "abc ^ def";
    "!x";
    "'a23";
  ] in
  List.iter (fun test_case ->
    let tokens = tokenize test_case in
    Printf.printf "Test case: %s\n" test_case;
    List.iter (fun token ->
      match token with
      | IDENTIFIER id -> Printf.printf "IDENTIFIER: %s\n" id
      | KEYWORD kw -> Printf.printf "KEYWORD: %s\n" kw
      | BOOLEAN b -> Printf.printf "BOOLEAN: %b\n" b
      | INTEGER i -> Printf.printf "INTEGER: %d\n" i
      | ARITH_OP op -> Printf.printf "ARITH_OP: %s\n" op
      | COMP_OP op -> Printf.printf "COMP_OP: %s\n" op
      | BOOL_OP op -> Printf.printf "BOOL_OP: %s\n" op
      | STRING_OP op -> Printf.printf "STRING_OP: %s\n" op
      | PAREN p -> Printf.printf "PAREN: %s\n" p
      | COMMA -> Printf.printf "COMMA\n"
      | ERROR msg -> Printf.printf "ERROR: %s\n" msg
    ) tokens;
    print_newline ()
  ) test_cases