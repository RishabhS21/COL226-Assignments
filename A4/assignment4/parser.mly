%{
    open Ast;;
%}

(* declarations *)

%token <string> VAR CONS ATOM
%token <int> NUM
%token LPAREN RPAREN LBRACE RBRACE QUERY COMMA SEMICOLON EQUAL NOT_EQ NOT ENDL CUT COND PIPE GT LT EOF

%left COMMA
%nonassoc EQUAL PIPE LT GT
%nonassoc ENDL

%start program
%type <Ast.program> program
%%


(* defining rules *)
(* The main entry point of the grammar, representing the whole program. *)

program:
    EOF                                 {[]}
  | clause_list EOF                     {$1}
;

clause_list:
    clause                              {[$1]}
  | clause clause_list                  {($1)::$2}
;

clause:
  | QUERY atom_list ENDL                {Query(Body($2))}
  | atom ENDL                           {Fact(Head($1))}
  | atom COND atom_list ENDL            {Rule(Head($1), Body($3))}
;

atom_list:
    atom                                {[$1]}
  | atom COMMA atom_list                {($1)::$3}
  | atom SEMICOLON atom_list            {($1)::$3}
;

atom:
  | CONS                                {Atomic_formula("CONSTANT (" ^ $1 ^ ")", [])}
  | VAR                                 {Atomic_formula("VARIABLE (" ^ $1 ^ ")", [])}
  | ATOM                                {Atomic_formula("ATOM (" ^ $1 ^ ")", [])}
  | CONS LPAREN term_list RPAREN        {Atomic_formula($1, $3)}
  | term EQUAL term                     {Atomic_formula("[_eq]", [$1; $3])}
  | term NOT_EQ term                    {Atomic_formula("[_not_eq]", [$1; $3])}
  | NOT term                            {Atomic_formula("[_not]", [$2])}
  | term LT term                        {Atomic_formula("[<]", [$1; $3])}
  | term GT term                        {Atomic_formula("[>]", [$1; $3])}
  | CUT                                 {Atomic_formula("[_cut]", [])}
;

term_list:
    term                                {[$1]}
  | term COMMA term_list                {($1)::$3}
  | term SEMICOLON term_list            {($1)::$3}
;

term:
    LPAREN term RPAREN                  {$2}
  | VAR                                 {Variable($1)}
  | CONS                                {Constant($1)}
  | NUM                                 {Num($1)}
  | ATOM                                {Atom($1)}
  | CONS LPAREN term_list RPAREN        {Node($1, $3)}
  | list                                {$1}
;

list:
    LBRACE RBRACE                        {Node("_empty_list", [])}
  | LBRACE list_elements RBRACE          {$2}
;

list_elements:
    term                                 {Node("_list", [$1])}
  | term COMMA list_elements             {Node("_list", [$1; $3])}
  | term PIPE list_elements              {Node("_list", [$1; $3])}
;