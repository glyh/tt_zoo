%{
  [@@@coverage exclued_file]
  open Ast
%}

%token EOF

%token <string> IDENT
%token <int> INT
%token <bool> BOOL

%token LPAREN RPAREN

%token LAMBDA IF THEN ELSE TBOOL TINT

%token COLON DOT ARROW EQ NE
%left EQ NE
%right ARROW

%start <ast> top_level

%%

top_level:
  | e=expression EOF { e }

expression: 
  | e=expression_stmt { e }

expression_stmt:
  | LAMBDA param=IDENT COLON ty=stlc_type DOT body=expression_stmt {
    Abs(param, ty, body)
  }
  | IF cond=expression_stmt THEN then_branch=expression_stmt ELSE else_branch=expression_stmt {
    If(cond, then_branch, else_branch)
  }
  | e=expression_expr { e }

expression_expr:
  | e=expression_relational { e }

%inline rel_op:
  | EQ { Eq }
  | NE { Ne }

expression_relational:
  | lhs=expression_relational op=rel_op rhs=expression_relational { 
    Binary(lhs, op, rhs)
  }
  | e=expression_app { e }

expression_app: 
  | f=expression_app x=expression_atom {
    App(f, x)
  }
  | e=expression_atom { e }

expression_atom:
  | sym=IDENT { Var(sym) }
  | i=INT { Atom(Int(i)) }
  | b=BOOL { Atom(Bool(b)) }
  | LPAREN e=expression RPAREN { e }

stlc_type:
  | TBOOL { TBool }
  | TINT { TInt }
  | lhs=stlc_type ARROW rhs=stlc_type { TArrow(lhs, rhs) }
  | LPAREN t=stlc_type RPAREN { t }


