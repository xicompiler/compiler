%{
  open Ast
%}

(* Keywords *)
%token USE
%token IF
%token ELSE
%token WHILE
%token RETURN
%token LENGTH

(* Literals *)
%token <Uchar.t> CHAR
%token <string> STRING
%token <string> INT
%token <bool> BOOL

(* Open/close delimeters *)
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token LBRACE
%token RBRACE

(* Assignment operator, [=] *)
%token GETS

(* Binary operators *)
%token MULT
%token HIGHMULT
%token DIV
%token MOD
%token PLUS
%token MINUS
%token LT
%token LEQ
%token GEQ
%token GT
%token EQ
%token NEQ

(* Logical Operators *)
%token NOT
%token AND
%token OR

(* Other punctuation *)
%token COLON
%token SEMICOLON
%token COMMA

(* Identifier *)
%token <string> ID
%token WILDCARD

(* End of file *)
%token EOF

(* A primitive type *)
%token <Type.primitive> TYPE
// %nonassoc IF
// %nonassoc ELSE

%start <Ast.t option> prog

%left OR
%left AND
%left EQ NEQ
%left LT LEQ GEQ GT
%left PLUS MINUS
%left MULT HIGHMULT DIV MOD
%nonassoc NOT NEG (* integer negation *)
%left APP INDEX (* function application, array indexing *)

%%

list_maybe_followed(X, TERM):
  | e = TERM?
    { Option.to_list e }
  | x = X; xs = list_maybe_followed(X, TERM)
    { x :: xs }

prog:
  | p = program; EOF { Some (Program p) }
  | i = interface; EOF { Some (Interface i) }
  | EOF { None }
  ;

program:
  | s = toplevel+ { s } 
  ;

toplevel:
  | s = toplevel_body; SEMICOLON? { s }
  ;
toplevel_body:
  | USE; id = ID; { Use id }
  | stmt = var_stmt; { Global stmt }
  | fn = fn; { Function fn } 
  ;

var_stmt:
  | assign = assign; { Var.Assign assign }
  | decl = decls; { Var.Decl decl }
  ;

assign:
  | a = separated_pair(assignees, GETS, exprs) { a }
  ;

assignees:
  | a = separated_nonempty_list(COMMA, assignee) { a }
  ;
assignee:
  | d = separated_pair(ID, COLON, typ) { Var.New d }
  | id = ID { Var.Existing id }
  | WILDCARD { Var.Wildcard }
  ;

exprs:
  | e = separated_nonempty_list(COMMA, expr) { e }
  ;
exprs_nullable:
  | e = separated_list(COMMA, expr) { e }
  ;
expr:
  | id = ID { Id id }
  | lit = literal { lit }
  | e = delimited(LPAREN, expr, RPAREN) { e }
  | e = delimited(LBRACE, exprs_nullable, RBRACE) { Array (Array.of_list e) }
  | e1 = expr; bop = binop; e2 = expr { Bop (bop, e1, e2) }
  | uop = unop; e = expr %prec NEG { Uop (uop, e) }
  | id = ID; e = app { App (id, e) }
  | builtin = builtin; e = app { App (builtin, e) }
  | e1 = expr; e2 = index { Index (e1, e2) } // TODO change ID back to expr
  ;

app:
  | e = delimited(LPAREN, exprs_nullable, RPAREN) %prec APP { e }
  ;

index:
  | e = delimited(LBRACKET, expr, RBRACKET) %prec INDEX { e }
  ;

literal:
  | c = CHAR { Int (c |> Uchar.to_int |> Int64.of_int) }
  | s = STRING { Array (array_of_string s) }
  | i = INT { Int 0L (* TODO process int *) }
  | b = BOOL { Bool b }
  ;

%inline binop:
  | MULT { Mult }
  | HIGHMULT { HighMult }
  | DIV { Div }
  | MOD { Mod }
  | PLUS { Plus }
  | MINUS { Minus }
  | LT { Lt }
  | LEQ { Leq}
  | GEQ { Geq }
  | GT { Gt }
  | EQ { Eq }
  | NEQ { Neq }
  | AND { And }
  | OR { Or }
  ;

%inline unop:
  | MINUS { IntNeg }
  | NOT { LogicalNeg }
  ;

builtin:
  | LENGTH { "length" }
  ;

decls:
  | d = separated_nonempty_list(COMMA, decl) { d }
  ;
decl:
  | d = separated_pair(ID, COLON, typ) { d } 
  ;

fn:
  | f = pair(signature, fn_body) { f }
  ;

signature:
  | id = ID; args = delimited(LPAREN, args, RPAREN); COLON; types = types { { id; args; types } }
  ;

args: 
  | a = separated_list(COMMA, decl) { a } 
  ;

types:
  | t = separated_nonempty_list(COMMA, typ) { t }
  ;

typ:
  | t = typ; LBRACKET; RBRACKET { Type.Array t }
  | t = TYPE { Type.Primitive t }
  ;

fn_body:
  | LBRACE; stmts = list_maybe_followed(stmt, return); RBRACE { stmts }
  ;

return:
  | RETURN; e = separated_list(COMMA, expr); SEMICOLON? { Return e }
  ;

stmt:
  | s = stmt_body; SEMICOLON? { s }
  ;
stmt_body:
  | b = delimited(LBRACE, stmt*, RBRACE) { Block b }
  | s = var_stmt { Var s }
  | IF; c = delimited(LPAREN, expr, RPAREN); s1 = stmt; s2 = preceded(ELSE, stmt)? { If (c, s1, s2) }
  | WHILE; c = delimited(LPAREN, expr, RPAREN); s = stmt { While (c, s) }
  | id = ID; e = delimited(LPAREN, exprs_nullable, RPAREN) { Proc (id, e) }
  ;

interface:
  | s = signature+ { s }
  ;