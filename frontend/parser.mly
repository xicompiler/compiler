%{
  module Pos = Node.Position
  open Core
  open Ast
  open Expr
  open Stmt
  open Position

  let int_err pos = raise (Exception.InvalidIntLiteral (get_position pos))
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
%token <Type.Tau.primitive> TYPE

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

(** [node(TERM)] is the pair [(e, start)] where [TERM] produces [e] and 
    [startpos] is the start position of [TERM] *)
node(TERM):
  | e = TERM
    { Pos.make ~pos:(get_position $startpos) e }
  ;

(** [enode] produces a node wrapping an [expr] *)
enode:
  | node = node(expr)
    { node }
  ;

(** [enodes] produces a comma-separated list of [enode]s *)
enodes:
  | nodes = separated_list(COMMA, enode)
    { nodes }
  ;

(** [snode] produces a node wrapping a statement *)
snode:
  | node = node(stmt)
    { node }
  ;

(** [list_maybe_followed(X, TERM)] recognizes either [x1; ...; xn] where 
    [xi : X] or [x1; ...; xn; term] where [term : TERM]. *)
list_maybe_followed(X, TERM):
  | e = TERM?
    { Option.to_list e }
  | x = X; xs = list_maybe_followed(X, TERM)
    { x :: xs }

(** [semi(X)] derives [X;] or [X] and produces [X] *)
semi(X):
  | x = X; SEMICOLON?
    { x }

(** [parens(X)] derives [(X)] and produces [X] *)
parens(X):
  | LPAREN; x = X; RPAREN
    { x }

(** [bracketed(X)] derives [[X]] and produces [X] *)
bracketed(X):
  | LBRACKET; x = X; RBRACKET
    { x }

(** A [binop] is a binary operator in Xi *)
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

(** A [unop] is a unary operator in Xi *)
%inline unop:
  | MINUS { IntNeg }
  | NOT { LogicalNeg }
  ;

(** A program is either a source or interface *)
program:
  | s = source { s }
  | i = interface { i }
  ;

(** A [source] derives a source file in  Xi, followed by EOF *)
source:
  | s = source_file; EOF
    { Source s }
  ;

(** An [interface] derives an interface file in Xi, followed by EOF  *)
interface:
  | signatures = signature+; EOF
    { Interface signatures }
  ;

(** A [source_file] derives a source file in  Xi *)
source_file:
  | definitions = definitions
    { { uses = []; definitions } }
  | uses = use+; definitions = definitions
    { { uses; definitions } }
  ;

(** [use] derives the top level statement [use id] *)
use:
  | USE; id = semi(ID)
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
  | decl = decl
    { GlobalDecl decl }
  | decl = decl; GETS; v = primitive
    { let (id, typ) = decl in GlobalInit (id, typ, v) }
  ;

decl:
  | id = ID; COLON; typ = typ
    { (id, typ) }
  ;

typ:
  | typ = TYPE
    { typ :> Type.tau }
  | typ = typ; LBRACKET; RBRACKET
    { `Array typ }
  ;

array_init:
  | base = typ; e = bracketed(enode); es = bracketed(enode?)*
    { 
      let es = Some e :: es in
      (List.fold_left ~f:(fun acc _ -> `Array acc) ~init:base es, es)
    }
  ;

index(lhs):
  | e1 = node(lhs); e2 = bracketed(enode)
    { (e1, e2) }
  ;

expr:
  | e1 = enode; bop = binop; e2 = enode
    { Bop (bop, e1, e2) }
  | e = uop_expr
    { e }
  ;

uop_expr:
  | uop = unop; e = node(uop_expr)
    {
      match uop, Pos.get e with
      | IntNeg, Primitive (Int i) ->
        if Int64.is_negative i then
          Uop (uop, e)
        else
          Primitive (Int (Int64.neg i))
      | IntNeg, Primitive IntBound ->
          Primitive (Int (Int64.min_value))
      | _ -> Uop (uop, e) 
    }
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
  | LENGTH; e = parens(enode)
    { Length e }
  | e = array_assign_expr
    { e }
  ;

primitive:
  | i = INT
    {
      try
        Int (Int64.of_string i)
      with _ -> begin
        try
          let _ = Int64.of_string ("-" ^ i) in
          IntBound
        with e ->
          int_err $startpos
      end
    }
  | b = BOOL
    { Bool b }
  | c = CHAR
    { Char c }
  ;

array:
  | e = enode?; RBRACE
    { Option.to_list e }
  | e = enode; COMMA; rest = array
    { e :: rest }
  ;

call:
  | id = ID; args = parens(enodes);
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
  | LBRACE; body = list_maybe_followed(snode, node(return)); RBRACE
    { body }
  ;

return:
  | RETURN; es = semi(enodes);
    { Return es }
  ;

stmt:
  | stmt = if_stmt
  | stmt = if_else
  | stmt = while_stmt
  | stmt = semi(semicolon_terminated)
    { stmt }
  ;

if_stmt:
  | IF; e = enode; s = snode
    { If (e, s) }
  ;

if_else:
  | IF; e = enode; s1 = snode; ELSE; s2 = snode
    { IfElse (e, s1, s2) }
  ;

while_stmt:
  | WHILE; e = enode; stmt = snode
    { While (e, stmt) }
  ;

separated_multiple_list(sep, X):
  | x = X; sep; xs = separated_nonempty_list(sep, X)
    { x :: xs }

semicolon_terminated:
  | decl = decl
    { VarDecl decl }
  | decl = decl; GETS; e = enode
    { let (id, typ) = decl in VarInit (id, typ, e) }
  | id = ID; COLON; init = array_init
    { let (typ, es) = init in ArrayDecl (id, typ, es) }
  | id = ID; GETS; e = enode
    { Assign (id, e) }
  | index = index(array_assign_lhs); GETS; e3 = enode
    { let (e1, e2) = index in ArrAssign (e1, e2, e3) }
  | ds = separated_multiple_list(COMMA, d); GETS; rhs = call
    { let (id, args) = rhs in MultiAssign (ds, id, args) }
  | WILDCARD; GETS; rhs = call
    { ExprStmt rhs }
  | call = call
    { PrCall call }
  | block = block
    { Block block }
  ;

d:
  | decl = decl
    { Some decl }
  | WILDCARD
    { None }
  ;
