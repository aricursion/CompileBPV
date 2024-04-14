%{
open Core
%}

%token Eof
%token Semicolon
%token <Int32.t> Dec_const
%token <Int32.t> Hex_const
%token <Symbol.t> Ident
%token <Symbol.t> TypIdent
%token Plus Minus Star Slash Percent Less Leq Greater Geq Equal Neq 
%token Logical_and Logical_or Bitwise_and Bitwise_xor Bitwise_or 
%token Shift_left, Shift_right 
%token Assign Plus_eq Minus_eq Star_eq Slash_eq Percent_eq Bitwise_and_eq Bitwise_xor_eq Bitwise_or_eq Shift_left_eq Shift_right_eq
%token L_brace R_brace
%token L_paren R_paren
%token L_bracket R_bracket
%token Unary
%token Logical_not Bitwise_not 
%token Minus_minus Plus_plus
%token Question Colon Comma
%token Dot Arrow
%token Struct Typedef If Else While For Continue Break Return Assert True False NULL Alloc Alloc_array Int Bool Void Char String 
%token Ternary
%token Asnop

(* Unary is a dummy terminal.
 * We need dummy terminals if we wish to assign a precedence
 * to a production that does not correspond to the precedence of
 * the rightmost terminal in that production.
 * Implicit in this is that precedence can only be inferred for
 * terminals. Therefore, don't try to assign precedence to "rules"
 * or "productions".
 *
 * Minus_minus is a dummy terminal to parse-fail on.
 *)

(* I thinkkkkk that you don't actually put parens in as an explicit precedence rule, 
 * but rather that they're baked into the grammar. For that reason they don't show up here
 * also I'm not really sure how to indicate the presence of the ternary operator 
 * what I'm going with for now is something similar to unary operators, where there's 
 * a dummy terminal that's used to indicate the presence of the ternary operator. 
 *)

%right Asnop
%right Ternary Question Colon 
%left Logical_or
%left Logical_and
%left Bitwise_or
%left Bitwise_xor
%left Bitwise_and
%left Equal Neq 
%left Leq Less Geq Greater
%left Shift_left Shift_right
%left Plus Minus
%left Star Slash Percent
%right Unary
%nonassoc Dot Arrow L_bracket R_bracket

%start program

(* It's only necessary to provide the type of the start rule,
 * but it can improve the quality of parser type errors to annotate
 * the types of other rules.
 *)
%type <Ast.program> program
%type <Ast.gdecl> gdecl
%type <Ast.param list> params
%type <Ast.param list> paramsfollow
%type <Ast.param> param
%type <Ast.mstm list> stms
%type <Ast.typ> typ
%type <Ast.retTyp> rtyp
%type <Ast.stm> stm
%type <Ast.mstm> m(stm)
%type <Ast.decl> decl
%type <Ast.simp> simp
%type <Ast.simp option> simpopt
%type <Ast.mstm option> elseopt
%type <Ast.control> control
%type <Ast.mexp option> mexpopt
%type <Ast.exp> exp
%type <Ast.postop> postop
%type <Ast.unop> unop
%type <Ast.mexp> m(exp)
%type <Ast.mexp list> arglist
%type <Ast.mexp list> arglistfollow
%type <Core.Int32.t> int_const
%type <Ast.binop> binop
%type <Ast.asnop> asnop

%%

program :
  | Eof; 
      { [] }
  | g = gdecl; p = program; 
      { g :: p }
  ;

gdecl : 
  | r = rtyp; name = Ident; L_paren; params = params; R_paren; Semicolon; 
      { Ast.Fdecl { retType = r; name = name; params = params } }
  | r = rtyp; name = Ident; L_paren; params = params; R_paren; L_brace; body = stms; R_brace; 
      { Ast.Fdefn { retType = r; name = name; params = params; body = body } }
  | Typedef; typ = typ; name = TypIdent; Semicolon; 
      { Ast.Typedef { typ = typ; name = name } }
  | Struct; name = eitherident; Semicolon; 
      { Ast.Sdecl name }
  | Struct; name = eitherident; L_brace; fields = fields; R_brace; Semicolon; 
      { Ast.Sdefn { name = name; fields = fields }}

param: 
  | typ = typ; ident = Ident; 
      { { ptyp = typ; name = ident } }

params: 
  | (* empty *)
      { [] }
  | param = param; pfollow = paramsfollow; 
      { param :: pfollow }
  
paramsfollow: 
  | (* empty *)
      { [] }
  | Comma; param = param; pfollow = paramsfollow; 
      { param :: pfollow }

field: 
  | typ = typ; ident = eitherident; Semicolon; 
      { { ftyp = typ; name = ident } }

fields: 
  | (* empty *)
      { [] }
  | f = field; fs = fields; 
      { f :: fs }

(* This higher-order rule produces a marked result of whatever the
 * rule passed as argument will produce.
 *)
m(x) :
  | x = x;
      (* $startpos(s) and $endpos(s) are menhir's replacements for
       * Parsing.symbol_start_pos and Parsing.symbol_end_pos, but,
       * unfortunately, they can only be called from productions. *)
      { mark x $startpos(x) $endpos(x) }
  ;

stms :
  | (* empty *)
      { [] }
  | hd = m(stm); tl = stms; 
    (* see https://dev.realworldocaml.org/parsing-with-ocamllex-and-menhir.html 
     * and in particular the section about menhir being a left recursive parser and taking linear time to read stms *)
      { hd :: tl }
  ;

stm :
  | s = simp; Semicolon;
      { Ast.Simp s }
  | d = control; 
      { Ast.Control d }
  | L_brace; body = stms; R_brace;
      { Ast.Block body }
  ;

rtyp : 
  | Void; 
      { Ast.Void }
  | typ = typ; 
      { Ast.Typ typ }

typ : 
  | Int;
      { Ast.Int }
  | Bool;
      { Ast.Bool }
  | ident = TypIdent;
      { Ast.TypIdent ident } 
  | typ = typ; Star; 
      { Ast.Pointer typ }
  | typ = typ; L_bracket; R_bracket; 
      { Ast.Array typ }
  | Struct; name = eitherident; 
      { Ast.Struct name}
  ;

eitherident: 
  | ident = TypIdent; 
        { ident }
  | ident = Ident; 
        { ident }

decl :
  | typ = typ; ident = Ident;
      { Ast.New_var (typ, ident) }
  | typ = typ; ident = TypIdent; 
      { Ast.New_var (typ, ident) }
  | typ = typ; ident = Ident; Assign; e = m(exp);
      { Ast.Init (typ, ident, e) }
  | typ = typ; ident = TypIdent; Assign; e = m(exp);
      { Ast.Init (typ, ident, e) }
  ;

simp :
  | lhs = m(exp);
    op = asnop;
    rhs = m(exp); %prec Asnop
      { Ast.Assign (lhs, op, rhs)}
  | lhs = m(exp); 
    op = postop; %prec Unary
      { Ast.Postfix (lhs, op) }
  | decl = decl; 
      { Ast.Declare decl }
  | exp = m(exp); 
      { Ast.Expression exp }
  ;

simpopt : 
  | (* empty *)
      { None }
  | simp = simp; 
      { Some simp}

(* the dangling else problem is solved (I think) by the fact that 
 * menhir always resolves shift/reduce conflicts in favor of shifting. 
 * This means that the else is always bound to the most recent possible if (I hope) *)
elseopt : 
  | (* empty *) 
      { None }
  | Else; else_branch = m(stm); 
      { Some else_branch}

control : 
  | If; 
    L_paren; 
    cond = m(exp);
    R_paren; 
    then_branch = m(stm); 
    else_branch = elseopt; 
      { Ast.If { cond = cond; then' = then_branch; else' = else_branch } }
  | While; 
    L_paren; 
    cond = m(exp); 
    R_paren;
    body = m(stm); 
      { Ast.While { cond = cond; body = body } }
  | For; 
    L_paren;
    init = simpopt; 
    Semicolon;
    cond = m(exp); 
    Semicolon;
    update = simpopt; 
    R_paren;
    body = m(stm); 
      { Ast.For { init = init; cond = cond; update = update; body = body } }
  | Return; e = mexpopt; Semicolon; 
      { Ast.Return e }
  | Assert; L_paren; e = m(exp); R_paren; Semicolon; 
      { Ast.Assert e }
  ; 

(* currently getting rid of lvalue and parsing lvalue as expressions, then fixing it in typechecking
lvalue :
  | ident = Ident;
      { ident }
  | L_paren; lhs = lvalue; R_paren;
      { lhs }
  ;
*) 

exp :
  | L_paren; e = exp; R_paren;
      { e }
  | c = int_const;
      { Ast.Const c }
  | True;
      { Ast.True }
  | False;
      { Ast.False }
  | NULL; 
      { Ast.NULL }
  | ident = Ident;
      { Ast.Var ident }
  | lhs = m(exp);
    op = binop;
    rhs = m(exp);
      { Ast.Binop { op = op; lhs = lhs; rhs = rhs } }
  | op = unop; e = m(exp); %prec Unary
      { Ast.Unop { op = op; operand = e; } }
  | cond = m(exp); 
    Question; 
    then_branch = m(exp); 
    Colon; 
    else_branch = m(exp); %prec Ternary
      { Ast.Ternop { cond = cond; then' = then_branch; else' = else_branch } }
  | ident = Ident; L_paren; arglist = arglist; R_paren; 
      { Ast.Function { name = ident; args = arglist } } 
  | structName = m(exp); Dot; field = eitherident; 
      { Ast.Dot { structName = structName; field = field } }
  | structAddr = m(exp); Arrow; field = eitherident; 
      { Ast.Arrow { structAddr = structAddr; field = field } }
  | Alloc; L_paren; typ = typ; R_paren; 
      { Ast.Alloc typ }
  | Star; pointer = m(exp); %prec Unary
      { Ast.Deref pointer }
  | Alloc_array; L_paren; typ = typ; Comma; size = m(exp); R_paren; 
      { Ast.AllocArray { typ = typ; size = size } }
  | arrayAddr = m(exp); L_bracket; index = m(exp); R_bracket; 
      { Ast.ArrayDeref { arrayAddr = arrayAddr; index = index } }
  ;

arglist : 
  | (* empty *)
      { [] }
  | arg = m(exp); afollow = arglistfollow; 
      { arg :: afollow }
  ;

arglistfollow :
  | (* empty *)
      { [] }
  | Comma; arg = m(exp); afollow = arglistfollow; 
      { arg :: afollow }
  ;

mexpopt : 
  | (* empty *)
      { None }
  | exp = m(exp); 
      { Some exp }

int_const :
  | c = Dec_const;
      { c }
  | c = Hex_const;
      { c }
  ;

(* See the menhir documentation for %inline.
 * This allows us to factor out binary operators while still
 * having the correct precedence for binary operator expressions.
 *)
%inline
binop :
  | Plus;
      { Ast.Plus }
  | Minus;
      { Ast.Minus }
  | Star;
      { Ast.Times }
  | Slash;
      { Ast.Divided_by }
  | Percent;
      { Ast.Modulo }
  | Less;
      { Ast.Less }
  | Leq;
      { Ast.Leq }
  | Greater;
      { Ast.Greater }
  | Geq;
      { Ast.Geq }
  | Equal;
      { Ast.Equal_equal }
  | Neq;
      { Ast.Neq }
  | Logical_and;
      { Ast.Logical_and }
  | Logical_or;
      { Ast.Logical_or }
  | Bitwise_and;
      { Ast.Bitwise_and }
  | Bitwise_xor;
      { Ast.Bitwise_xor }
  | Bitwise_or;
      { Ast.Bitwise_or }
  | Shift_left;
      { Ast.Shift_left }
  | Shift_right;
      { Ast.Shift_right }
  ;

unop : 
  | Minus;
      { Ast.Negative }
  | Logical_not;
      { Ast.Logical_not }
  | Bitwise_not;
      { Ast.Bitwise_not }
  ;

postop : 
  | Plus_plus;
      { Ast.Increment }
  | Minus_minus;
      { Ast.Decrement }

asnop :
  | Assign
    { Ast.Equal }
  | Plus_eq
    { Ast.Plus_equal }
  | Minus_eq
    { Ast.Minus_equal }
  | Star_eq
    { Ast.Times_equal }
  | Slash_eq
    { Ast.Divide_equal }
  | Percent_eq
    { Ast.Modulo_equal }
  | Bitwise_and_eq
    { Ast.Bitwise_and_equal }
  | Bitwise_xor_eq
    { Ast.Bitwise_xor_equal }
  | Bitwise_or_eq
    { Ast.Bitwise_or_equal }
  | Shift_left_eq
    { Ast.Shift_left_equal }
  | Shift_right_eq
    { Ast.Shift_right_equal }
  ;

%%
