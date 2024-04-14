%token Eof
%token <int> Dec_const
%token <Variable.t> Var
%token <string> String
%token Plus Star Arrow_typ Unit Void
%token L_brace R_brace
%token L_paren R_paren
%token L_bracket R_bracket
%token L_staple R_staple
%token Pipe
%token Colon Semicolon Comma Quote
%token Lambda Arrow
%token Print Inj Split Case As In

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

prods: 
  | (* empty *)
      { [] }
  | m = term; prodFollow = prodsFollow; 
      { m :: prodFollow }
  
prodsFollow: 
  | (* empty *)
      { [] }
  | Comma; m = term; prodFollow = prodsFollow; 
      { m :: prodFollow }

vars: 
  | (* empty *)
      { [] }
  | var = Var; varFollow = varsFollow; 
      { var :: varFollow }

varsFollow: 
  | (* empty *)
      { [] }
  | Comma; var = Var; varFollow = varsFollow; 
      { var :: varFollow }

cases: 
  | (* empty *)
      { [] }
  | var = Var; Arrow; m = term; caseFollow = casesFollow;
      { (var, m) :: caseFollow }
  
casesFollow: 
  | (* empty *)
      { [] }
  | Pipe; var = Var; Arrow; m = term; caseFollow = casesFollow; 
      { (var, m) :: caseFollow }


prodTyplist : 
  | (* empty *)
      { [] }
  | typ = typ; typFollow = prodTypListFollow; 
      { typ :: typFollow }
  ;

prodTypListFollow :
  | (* empty *)
      { [] }
  | Star; typ = typ; typFollow = prodTypListFollow; 
      { typ :: typFollow }
  ;

sumTyplist : 
  |
      { [] }
  | typ = typ; typFollow = sumTypListFollow; 
      { typ :: typFollow }
  ;

sumTypListFollow :
  |
      { [] }
  | Plus; typ = typ; typFollow = sumTypListFollow; 
      { typ :: typFollow }
  ;

typ : 
  | Unit;
      { Ast.Prod [] }
  | Void;
      { Ast.Sum [] }
  (* asserting that products/sums are either nullary or at least binary *)
  | t = typ; Star; l = prodTyplist
      { Ast.Prod (t::l) }
  | t = typ; Plus; l = sumTyplist; 
      { Ast.Sum (t::l) }
  | t1 = typ; Arrow_typ; t2 = typ; 
      { Ast.Arrow (t1, t2) }
  ;

term :
  | L_paren; m = term; R_paren;
      { m }
  | var = Var;
      { Ast.Var var }
  | L_bracket; prods = prods; R_bracket; 
      { Ast.Tup prods }
  | Split; m = term; As; vars = vars; In; body = term
      { Ast.Split (m, (vars, body)) }
  | Lambda; L_paren; var = Var; Colon; typ = typ; R_paren; Arrow; body = term;
      { Ast.Lambda (var, typ, body) }  
  | L_paren; m = term; R_paren; m1 = term;
      { Ast.Ap (m, m1) } 
  | Inj; L_staple; typ = typ; R_staple; L_paren; i = Dec_const; R_paren; L_paren; m = term; R_paren; 
      { Ast.Inj (typ, i, m) }
  | Case; m = term; L_brace; cases = cases; R_brace; 
      { Ast.Case (m, cases) }
  | Print; Quote; s = String; Quote; Semicolon; m = term; 
      { Ast.Print (s, m) }
  ;

%%
