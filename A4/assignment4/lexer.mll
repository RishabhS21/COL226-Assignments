{
  open Parser;;
}

(* 
* tokenizing all possible standard tokens
*)

let variable = ['A'-'Z' '_']['A'-'Z' 'a'-'z' '0'-'9' '_']*
let constant = ['a'-'z']['A'-'Z' 'a'-'z' '0'-'9' '_']* | ("\"" [^ '\"']+ "\"")
let at = ("'" [^ '\"']+ "'")
let space_chars = [' ' '\t' '\n']+
let number = '0'|['1'-'9']['0'-'9']*
let bool = "true" | "false"
let query = "?-"
let lparen = '('
let rparen = ')'
let lbrace = '['
let rbrace = ']'
let comma = ','
let semicolon = ';'
let equal = '='
let gt = '>'
let lt = '<'
let not_eq = "\\="
let not = "\\+"
let pipe = '|'
let cut = '!'
let endl = '.'
let cond = ":-"

rule tokenize = parse
    eof                   {EOF}
  | space_chars           {tokenize lexbuf}
  | variable as v         {VAR(v)}
  | constant as c         {CONS(c)} 
  | at as atm             {ATOM(atm)}
  | number as n           {NUM(int_of_string n)}
  | query                 {QUERY}
  | lparen                {LPAREN}
  | rparen                {RPAREN}
  | lbrace                {LBRACE}
  | rbrace                {RBRACE}
  | comma                 {COMMA}
  | semicolon             {SEMICOLON}
  | equal                 {EQUAL}
  | gt                    {GT}
  | lt                    {LT}
  | not_eq                {NOT_EQ}
  | not                   {NOT}
  | pipe                  {PIPE}
  | cut                   {CUT}
  | endl                  {ENDL}
  | cond                  {COND}
  | '%'                   {oneline_comment lexbuf}
  | "/*"                  {multiline_comment lexbuf}

and oneline_comment = parse
    eof                   {EOF}
  | '\n'                  {tokenize lexbuf}
  |   _                   {oneline_comment lexbuf}

and multiline_comment = parse
    eof                   {failwith "ERROR: End of file without closing the comment"}
  | "*/"                  {tokenize lexbuf}
  |  _                    {multiline_comment lexbuf}