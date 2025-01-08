%{
  [@@@coverage exclued_file]
  open Data_type
%}

%token EOF

%token <string> IDENT

%token LPAREN RPAREN LBKT RBKT

%token COLON SEMICOL FSLASH ARROW DOT EQ HOLE
%right ARROW

%token LET U

%start <raw> top_level

%%

top_level:
  | e=expression EOF { e }

expression: 
  | e=expression_stmt { e }

expression_stmt:
  | LET sym=IDENT COLON ty=expression_stmt EQ rhs=expression_stmt SEMICOL body=expression_stmt {
    RLet(sym, ty, rhs, body)
  }
  | FSLASH params=list(IDENT) DOT body=expression_stmt {
    List.fold_right (fun param body -> RLam(param, body)) params body
  }
  | e=expression_expr { e }

expression_expr:
  (* to avoid shift/reduce conflicts *)
  | LBKT params=list(IDENT) COLON ty=expression_expr RBKT ARROW body=expression_expr {
    List.fold_right (fun param body -> RPi(param, ty, body)) params body
  }
  | lhs=expression_expr ARROW rhs=expression_expr {
    RPi("_", lhs, rhs)
  }
  | e=expression_app { e }

expression_app: 
  | f=expression_app x=expression_atom {
    RApp(f, x)
  }
  | e = expression_atom { e }

expression_atom:
  | U { RU }
  | HOLE { RHole }
  | var=IDENT { RVar(var) }
  | LPAREN e=expression RPAREN { e }
