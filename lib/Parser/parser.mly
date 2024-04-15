%token Eof
%token <int> Dec_const
%token <Variable.t> Var
%token <string> String
%token Plus Star Arrow_typ Unit
%token L_brace R_brace
%token L_paren R_paren
%token L_bracket R_bracket
%token L_staple R_staple
%token Pipe
%token Colon Semicolon Comma
%token Lambda Arrow
%token Print Inj Split Case As In Check

%right Arrow_typ

%start program

%type <Ast.term> term
%type <Ast.typ> typ
%type <Ast.term> program

%%

program:
  | m = term;
  Eof;
  { m }
  ;

typ : 
  | Unit;
      { Ast.Unit }
  | t1 = typ; Star; t2 = typ;
      { Ast.Prod (t1, t2) }
  | t1 = typ; Plus; t2 = typ; 
      { Ast.Sum (t1, t2) }
  | t1 = typ; Arrow_typ; t2 = typ; 
      { Ast.Arrow (t1, t2) }
  ;

term :
  | L_paren; m = term; R_paren;
      { m }
  | var = Var;
      { Ast.Var var }
  | L_bracket; R_bracket; 
      { Ast.Triv }  
  | L_bracket; m1 = term; Comma; m2 = term; R_bracket; 
      { Ast.Tup (m1, m2) }
  | Split; m = term; As; v1 = Var; Comma; v2 = Var; In; body = term
      { Ast.Split (m, ((v1, v2), body)) }
  | Lambda; L_paren; var = Var; Colon; typ = typ; R_paren; Arrow; body = term;
      { Ast.Lambda (var, typ, body) }  
  | L_paren; m = term; R_paren; m1 = term;
      { Ast.Ap (m, m1) } 
  | Inj; L_staple; typ = typ; R_staple; L_paren; i = Dec_const; R_paren; L_paren; m = term; R_paren; 
      { Ast.Inj (typ, i, m) }
  | Case; m = term; L_brace; v1 = Var; Arrow; m1 = term; Pipe; v2 = Var; Arrow; m2 = term; R_brace; 
      { Ast.Case (m, (v1, m1), (v2, m2)) }
  | Check; m = term; In; body = term; 
      { Ast.Check (m, body) }  
  | Print; s = String; Semicolon; m = term; 
      { Ast.Print (s, m) }
  ;

%%
