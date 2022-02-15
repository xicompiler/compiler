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

%start <Ast.t> start

%left OR
%left AND
%left EQ NEQ
%left LT LEQ GEQ GT
%left PLUS MINUS
%left MULT HIGHMULT DIV MOD

%nonassoc IF
%nonassoc ELSE

%%

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

start:
  | p = program; EOF { p }
  ;

program:
  | s = source
    { s }
  | i = interface
    { i }
  ;

source:
  | definitions = definitions
    { Source { uses = []; definitions } }
  | uses = use+; definitions = definitions
    { Source { uses; definitions } }
  ;

interface:
  | signatures = signature+
    { Interface signatures }
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
  | init = init
    { GlobalInit init }
  ;

decl:
  | id = ID; COLON; t = typ
    { (id, t) }
  ;

typ:
  | t = TYPE
    { Type.Primitive t }
  | t = typ; LBRACKET; RBRACKET
    { Type.Array t }
  ;

init:
  | init = separated_pair(decl, GETS, expr)
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
  | i = INT
    { Int i }
  | b = BOOL
    { Bool b }
  | LBRACE; array = array
    { Array (Array.of_list array) }
  | s = STRING;
    { String s }
  | id = ID
    { Id id }
  ;

array:
  | e = expr?; RBRACE
    { Option.to_list e }
  | e = expr; COMMA; rest = array
    { e :: rest }
  ;

call:
  | id = ID; LPAREN; args = separated_list(COMMA, expr); RPAREN
    { (id, args) }
  ;

fn:
  | signature = signature; body = block
    { { signature; body } }
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
  | LBRACE; body = stmt*; return = return?; RBRACE
    { { body; return } }
  ;

return:
  | RETURN; e = expr?; SEMICOLON?
    { e }
  ;

stmt:
  | stmt = if_stmt
  | stmt = while_stmt
  | stmt = single_stmt; SEMICOLON?
    { stmt }
  | block = block
    { Block block }

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

single_stmt:
  | decl = decl
    { Decl decl }
  | init = init
    { Init init }
  | id = ID; GETS; e = expr
    { Assign (id, e) }
  | lhs = separated_multiple_list(COMMA, multi_assignee); GETS; rhs = call
    { MultiInit (lhs, rhs) }
  | call = call
    { ProcCall call }
  ;

multi_assignee:
  | decl = decl
    { Var decl }
  | WILDCARD
    { Wildcard }
  ;
