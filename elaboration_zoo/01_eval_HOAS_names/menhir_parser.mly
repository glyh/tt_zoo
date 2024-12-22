%{
  [@@@coverage exclued_file]
  open Ast
%}

%token EOF

%token <string> IDENT

%token LPAREN RPAREN

%token LET EQ FSLASH DOT SEMICOL

%start <term> top_level

%%

top_level:
  | e=expression EOF { e }

expression: 
  | e=expression_stmt { e }

expression_stmt:
  | LET sym=IDENT EQ rhs=expression_stmt SEMICOL body=expression_stmt {
    Let(sym, rhs, body)
  }
  | FSLASH params=list(IDENT) DOT body=expression_stmt {
    List.fold_right (fun param body -> Lam(param, body)) params body
  }
  | e=expression_expr { e }

expression_expr:
  | f=expression_expr x=expression_atom {
    App(f, x)
  }
  | e=expression_atom { e }

expression_atom:
  | sym=IDENT { Var(sym) }
  | LPAREN e=expression RPAREN { e }



