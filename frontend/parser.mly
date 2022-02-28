%{
  open Core
  open Ast
  open Expr
  open Stmt
  open Position
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
%token <Ast.Tau.primitive> TYPE

%start <Ast.t> program
%start <Ast.t> source
%start <Ast.t> interface

%left OR
%left AND
%left EQ NEQ
%left LT LEQ GEQ GT
%left PLUS MINUS
%left MULT HIGHMULT DIV MOD

%nonassoc IF
%nonassoc ELSE

%%

node(TERM):
  | e = TERM
    { (e, get_position $startpos) }
  ;

list_maybe_followed(X, TERM):
  | e = TERM?
    { Option.to_list e }
  | x = X; xs = list_maybe_followed(X, TERM)
    { x :: xs }
  ;

semi(X):
  | x = X; SEMICOLON?
    { x }

parens(X):
  | LPAREN; x = X; RPAREN
    { x }

(** [epsilon] derives the empty string *)
epsilon:
  | (* nothing *)
    { None }
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

program:
  | s = source { s }
  | i = interface { i }
  ;

source:
  | s = source_entry; EOF
    { Source s }
  ;

interface:
  | i = interface_entry; EOF
    { Interface i }
  ;

source_entry:
  | definitions = definitions
    { { uses = []; definitions } }
  | uses = use+; definitions = definitions
    { { uses; definitions } }
  ;

interface_entry:
  | signatures = signature+
    { signatures }
  ;

use:
  | USE; id = semi(ID);
    { id }
  ;

definitions:
  | global = global_semi; definitions = definitions
    { global :: definitions }
  | fn_defn = fn_defn; definitions = definition*
    { fn_defn :: definitions }
  ;

definition:
  | defn = global_semi
  | defn = fn_defn
    { defn }
  ;

global_semi:
  | global = semi(global)
    { global }
  ;

fn_defn:
  | fn = fn
    { FnDefn fn }
  ;

global:
  | decl = global_decl
    { GlobalDecl decl }
  | decl = global_decl; GETS; v = primitive
    { GlobalInit (decl, v) }
  ;

global_decl:
  | decl = decl_length(epsilon)
    { decl }
  ;

decl:
  | decl = decl_length(expr?)
    { decl }
  ;

decl_length(length):
  | id = ID; COLON; t = typ_length(length)
    { (id, t) }

typ:
  | t = typ_length(expr?)
    { t }
  ;

typ_length(length):
  | t = TYPE; array_type = loption(array_type(length))
    { List.fold_left ~f:Tau.array ~init:(t :> Tau.t) array_type }

array_type(length):
  | t = loption(array_type(length)); LBRACKET; length = length; RBRACKET
    { length :: t }

init:
  | init = separated_pair(init_target, GETS, expr)
    { init }
  ;

index(lhs):
  | e1 = node(lhs); LBRACKET; e2 = expr; RBRACKET
    { (e1, e2) }
  ;

expr:
  | e1 = node(expr); bop = binop; e2 = node(expr)
    { Bop (bop, e1, e2) }
  | e = uop_expr
    { e }
  ;

uop_expr:
  | uop = unop; e = node(uop_expr)
    { Uop (uop, e) }
  | e = call_expr
    { e }
  ;

call_expr:
  | index = index(call_expr)
    { Index index }
  | e = parens(expr)
    { e }
  | v = primitive
    { Primitive v }
  | LBRACE; array = array
    { Array (Array.of_list array) }
  | s = STRING
    { String s }
  | LENGTH; e = parens(expr)
    { Length e }
  | e = array_assign_expr
    { e }
  ;

primitive:
  | i = INT
    { Int i }
  | b = BOOL
    { Bool b }
  | c = CHAR
    { Char c }
  ; 

array:
  | e = node(expr)?; RBRACE
    { Option.to_list e }
  | e = node(expr); COMMA; rest = array
    { e :: rest }
  ;

call:
  | id = ID; args = parens(separated_list(COMMA, node(expr)));
    { (id, args) }
  ;

(** [array_assign_expr] is any non-array-index expression [e1] that can appear 
    in the statement [e1[e2] = e3] *)
array_assign_expr:
  | call = call
    { FnCall call }
  | id = ID
    { Id id }
  ;

(** [array_assign_lhs] is any expression e1 that can appear in the statement 
    [e1[e2] = e3] *)
array_assign_lhs:
  | index = index(array_assign_lhs)
    { Index index }
  | e = array_assign_expr
    { e }

fn:
  | signature = signature; body = block
    { (signature, body) }
  ;

signature:
  | id = ID; LPAREN; params = params; RPAREN; types = loption(types)
    { { id; params; types } }
  ;

params:
  | params = separated_list(COMMA, decl)
    { params }
  ;

types:
  | COLON; types = separated_nonempty_list(COMMA, typ)
    { types }
  ;

block:
  | LBRACE; body = list_maybe_followed(stmt, return); RBRACE
    { body }
  ;

return:
  | RETURN; es = semi(separated_list(COMMA, node(expr)));
    { Return es }
  ;

stmt:
  | stmt = if_stmt
  | stmt = while_stmt
  | stmt = semi(semicolon_terminated);
    { stmt }
  ;

if_stmt:
  | IF; e = node(expr); stmt1 = node(stmt); stmt2 = ioption(node(else_stmt))
    { If (e, stmt1, stmt2) }
  ;

%inline else_stmt:
  | ELSE; stmt = stmt
    { stmt }
  ;

while_stmt:
  | WHILE; e = node(expr); stmt = node(stmt)
    { While (e, stmt) }
  ;

separated_multiple_list(sep, X):
  | x = X; sep; xs = separated_nonempty_list(sep, X)
    { x :: xs }

semicolon_terminated:
  | decl = decl
    { Decl decl }
  | init = init
    { Init init }
  | target = assign_target; GETS; e = expr
    { Assign (target, e) }
  | lhs = separated_multiple_list(COMMA, init_target); GETS; rhs = call
    { MultiInit (lhs, rhs) }
  | call = call
    { PrCall call }
  | block = block
    { Block block }
  ;

assign_target:
  | id = ID
    { Var id }
  | index = index(array_assign_lhs)
    { ArrayElt index }
  ;

init_target:
  | decl = decl
    { InitDecl decl }
  | WILDCARD
    { Wildcard }
  ;
