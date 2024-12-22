%{
  [@@@coverage exclued_file]
  open Ast
%}

%token EOF

%token <int> INDEX

%token LPAREN RPAREN

%token LET SEMICOL LAMBDA

%start <term> top_level

%%

top_level:
  | e=expression EOF { e }

expression: 
  | e=expression_stmt { e }

expression_stmt:
  | LET rhs=expression_stmt SEMICOL body=expression_stmt {
    Let(rhs, body)
  }
  | LAMBDA body=expression_stmt {
    Lam(body)
  }
  | e=expression_expr { e }

expression_expr:
  | f=expression_expr x=expression_atom {
    App(f, x)
  }
  | e=expression_atom { e }

expression_atom:
  | idx=INDEX { Var(idx) }
  | LPAREN e=expression RPAREN { e }



