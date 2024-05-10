%token Eof
%token <int> Dec_const
%token <Variable.t> Var
%token <string> String
%token Plus Star Arrow_typ Unit Int String_Typ
%token L_brace R_brace
%token L_paren R_paren
%token L_bracket R_bracket
%token L_staple R_staple
%token Pipe
%token Colon Comma
%token Lambda Arrow
%token Print Inj Split Case As In Check Div Sub IntToString Concat Let Equal

%right Arrow_typ
%right Arrow
%right In
%left Plus
%left Sub 
%left Star

%right Print
%left Div
%left Concat

%right IntToString


%start program

%type <Ast.term> term
%type <Ast.typ> typ
%type <Ast.term> program
%type <Ast.term> atom
%type <Ast.term> appTerm

%%

program:
  | m = term;
  Eof;
  { m }
  ;

typ : 
  | Unit;
      { Ast.Unit }
  | L_paren; t = typ; R_paren;
      { t }
  | t1 = typ; Star; t2 = typ;
      { Ast.Prod (t1, t2) }
  | t1 = typ; Plus; t2 = typ; 
      { Ast.Sum (t1, t2) }
  | t1 = typ; Arrow_typ; t2 = typ; 
      { Ast.Arrow (t1, t2) }
  | Int;
      { Ast.Int_Typ }
  | String_Typ;
      {Ast.String_Typ }
  ;

appTerm :
  | a = atom;
    { a }
  | m = appTerm ; m1 = atom;
    { Ast.Ap (m, m1) }

atom :
  | L_paren; m = term; R_paren;
      { m }
  | var = Var;
      { Ast.Var var }
  | n = Dec_const ;
      {Ast.Int n }
  | s = String ;
      {Ast.String s}
  | L_bracket; R_bracket;
      { Ast.Triv }

term :
  | m = appTerm;
      { m }
  | L_bracket; m1 = term; Comma; m2 = term; R_bracket; 
      { Ast.Tup (m1, m2) }
  | Split; m = term; As; v1 = Var; Comma; v2 = Var; In; body = term
      { Ast.Split (m, ((v1, v2), body)) }
  | Lambda; L_paren; var = Var; Colon; typ = typ; R_paren; Arrow; body = term;
      { Ast.Lambda (var, typ, body) }  
  | Inj; L_staple; typ = typ; R_staple; L_paren; i = Dec_const; R_paren; L_paren; m = term; R_paren; 
      { Ast.Inj (typ, i, m) }
  | Case; m = term; L_brace; v1 = Var; Arrow; m1 = term; Pipe; v2 = Var; Arrow; m2 = term; R_brace; 
      { Ast.Case (m, (v1, m1), (v2, m2)) }
  | Check; m = term; In; body = term; 
      { Ast.Check (m, body) }  
  | Print; t = term; 
      { Ast.Prim (Prim.Print, [t]) }
  | m = term; Plus; n = term;
      { Ast.Prim (Prim.Add, [m; n])}
  | m = term; Sub; n = term;
      { Ast.Prim (Prim.Add, [m; n])}
  | m = term; Div; n = term;
      { Ast.Prim (Prim.Div, [m; n])}
  | m = term; Star; n = term;
      { Ast.Prim (Prim.Mul, [m; n])}
  | IntToString; m = term;
      {Ast.Prim (Prim.IntToString, [m])}
  | m = term; Concat; n = term;
      {Ast.Prim (Prim.Concat, [m; n])}
  | Let x = Var; Equal; m = term; In; n = term;
      {Ast.Let (x, m, n)}
  ;

%%
