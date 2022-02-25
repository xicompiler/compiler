%{
  open Core
  open Ast
  open Expr
  open Stmt
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
%token <Ast.Type.primitive> TYPE

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

list_maybe_followed(X, TERM):
  | e = TERM?
    { Option.to_list e }
  | x = X; xs = list_maybe_followed(X, TERM)
    { x :: xs }

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
  | USE; id = ID; SEMICOLON?
    { id }
  ;

definitions:
  | global = global; SEMICOLON?; definitions = definitions
    { global :: definitions }
  | fn = fn; definitions = loption(definitions)
    { FnDefn fn :: definitions }
  ;

global:
  | decl = decl
    { GlobalDecl decl }
  | decl = decl; GETS; v = primitive
    { GlobalInit (decl, v) }
  ;

decl:
  | id = ID; COLON; t = typ
    { (id, t) }
  ;

typ:
  | t = TYPE; array_type = loption(array_type)
    { List.fold_left ~f:Type.array ~init:(Type.Primitive t) array_type }
  ;

array_type:
  | t = loption(array_type); LBRACKET; length = expr?; RBRACKET
    { length :: t }
  ;

init:
  | init = separated_pair(init_target, GETS, expr)
    { init }
  ;

expr:
  | e1 = expr; bop = binop; e2 = expr
    { Bop (bop, e1, e2) }
  | e = uop_expr
    { e }
  ;

uop_expr:
  | uop = unop; e = uop_expr
    { Uop (uop, e) }
  | e = call_expr
    { e }
  ;

call_expr:
  | e1 = call_expr; LBRACKET; e2 = expr; RBRACKET
    { Index (e1, e2) }
  | call = call
    { FnCall call }
  | LPAREN; e = expr; RPAREN
    { e }
  | v = primitive
    { Primitive v }
  | s = STRING
    { String s }
  | LBRACE; array = array
    { Array (Array.of_list array) }
  | id = ID
    { Id id }
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
  | e = expr?; RBRACE
    { Option.to_list e }
  | e = expr; COMMA; rest = array
    { e :: rest }
  ;

call:
  | id = callee; LPAREN; args = separated_list(COMMA, expr); RPAREN
    { (id, args) }
  ;

callee:
  | id = ID
    { id }
  | LENGTH
    { "length" }
  ;

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
  | RETURN; es = separated_list(COMMA, expr); SEMICOLON?
    { Return es }
  ;

stmt:
  | stmt = if_stmt
  | stmt = while_stmt
  | stmt = semicolon_terminated; SEMICOLON?
    { stmt }
  ;

if_stmt:
  | IF; e = expr; stmt1 = stmt; stmt2 = ioption(else_stmt)
    { If (e, stmt1, stmt2) }
  ;

%inline else_stmt:
  | ELSE; stmt = stmt
    { stmt }
  ;

while_stmt:
  | WHILE; e = expr; stmt = stmt
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
  | target = assign_target; LBRACKET; e = expr; RBRACKET
    { ArrayElt (target, e) }
  ;

init_target:
  | decl = decl
    { InitDecl decl }
  | WILDCARD
    { Wildcard }
  ;
