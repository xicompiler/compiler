%{
  module Pos = Node.Position
  open Core
  open Ast
  open Expr
  open Stmt
  open Toplevel
  open Position

  let node ~pos t = Pos.make ~pos:(get_position pos) t

  let int_err pos i = raise (Exception.InvalidIntLiteral (get_position pos, i))

  type unsafe_int =
    | SafeInt of Int64.t
    | IntBound of string

  type unsafe_primitive =
    | SafePrimitive of primitive
    | UnsafeInt of unsafe_int
  
  type unsafe_expr =
    | SafeExpr of expr
    | UnsafePrimitive of unsafe_primitive
  
  let int_of_unsafe ~pos = function
    | SafeInt i -> i
    | IntBound s -> int_err pos s

  let primitive_of_unsafe ~pos = function
    | SafePrimitive p -> p
    | UnsafeInt i -> Int (int_of_unsafe ~pos i)

  let expr_of_unsafe ~pos = function
    | SafeExpr p -> p
    | UnsafePrimitive p -> Primitive (primitive_of_unsafe ~pos p)
  
  let enode_of_unsafe ~pos e = node ~pos (expr_of_unsafe ~pos e)

  let parse_int ~pos ~neg s =
    try
      let i = if neg then "-" ^ s else s in
      SafeInt (Int64.of_string i)
    with _ ->
      int_err pos s
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

%start <Ast.t> prog
%start <Ast.Toplevel.source> source
%start <Ast.Toplevel.intf> intf

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
  | t = TERM
    { node $startpos t }
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

(** A prog is either a source or intf *)
prog:
  | s = source { Source s }
  | i = intf { Intf i }
  ;

(** A [source] derives a source file in  Xi, followed by EOF *)
source:
  | s = source_file; EOF
    { s }
  ;

(** An [intf] derives an intf file in Xi, followed by EOF  *)
intf:
  | signatures = node(signature)+; EOF
    { signatures }
  ;

(** A [source_file] derives a source file in  Xi *)
source_file:
  | definitions = definitions
    { { uses = []; definitions } }
  | uses = node(use)+; definitions = definitions
    { { uses; definitions } }
  ;

id:
  | id = node(ID)
    { id }
  ;

(** [use] derives the top level statement [use id] *)
use:
  | USE; id = semi(id)
    { id }
  ;

definitions:
  | global = node(global_semi); definitions = definitions
    { global :: definitions }
  | fn_defn = node(fn_defn); definitions = node(definition)*
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
    { 
      let (id, typ) = decl in
      let v = primitive_of_unsafe ~pos:$startpos(v) v in
      GlobalInit (id, typ, v)
    }
  | decl = decl; GETS; neg = MINUS; i = INT
    {
      let (id, typ) = decl in 
      let pos = $startpos(neg) in
      let i = parse_int ~pos ~neg:true i in
      let v = Int (int_of_unsafe ~pos i) in
      GlobalInit (id, typ, v)
    }
  ;

decl:
  | id = id; COLON; typ = typ
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
  | e1 = lhs; e2 = bracketed(enode)
    { (enode_of_unsafe ~pos:$startpos e1, e2) }
  ;

expr:
  | e = bop_expr
    { expr_of_unsafe ~pos:$startpos(e) e }
  ;

bop_expr:
  | e1 = enode; bop = binop; e2 = enode
    { SafeExpr (Bop (bop, e1, e2)) }
  | e = uop_expr
    { e }
  ;

uop_expr:
  | uop = unop; e = uop_expr
    {
      let pos = $startpos(e) in
      match uop, e with
      | IntNeg, SafeExpr (Primitive (Int i)) ->
        if Int64.is_negative i then
         SafeExpr (Uop (uop, (enode_of_unsafe ~pos e)))
        else
          UnsafePrimitive (UnsafeInt (SafeInt (Int64.neg i)))
      | IntNeg, UnsafePrimitive (UnsafeInt (IntBound _)) ->
          UnsafePrimitive (UnsafeInt (SafeInt (Int64.min_value)))
      | _ -> 
         SafeExpr (Uop (uop, (enode_of_unsafe ~pos e)))
    }
  | e = call_expr
    { e }
  ;

call_expr:
  | index = index(call_expr)
    { SafeExpr (Index index) }
  | e = parens(expr)
    { SafeExpr e }
  | v = primitive
    { UnsafePrimitive v }
  | LBRACE; array = array
    { SafeExpr (Array (Array.of_list array)) }
  | s = STRING
    { SafeExpr (String s) }
  | LENGTH; e = parens(enode)
    { SafeExpr (Length e) }
  | e = array_assign_expr
    { SafeExpr e }
  ;

primitive:
  | i = unsafe_int
    { UnsafeInt i }
  | b = BOOL
    { SafePrimitive (Bool b) }
  | c = CHAR
    { SafePrimitive (Char c) }
  ;

unsafe_int:
  | i = INT
    {
      let pos = $startpos in
      try
        parse_int ~pos ~neg:false i
      with _ ->
        let _ = parse_int ~pos ~neg:true i in
        IntBound i
    }
  ;

array:
  | e = enode?; RBRACE
    { Option.to_list e }
  | e = enode; COMMA; rest = array
    { e :: rest }
  ;

call:
  | id = id; args = parens(enodes);
    { (id, args) }
  ;

(** [array_assign_expr] is any non-array-index expression [e1] that can appear 
    in the statement [e1[e2] = e3] *)
array_assign_expr:
  | call = call
    { FnCall call }
  | id = id
    { Id id }
  ;

(** [array_assign_lhs] is any expression e1 that can appear in the statement 
    [e1[e2] = e3] *)
array_assign_lhs:
  | index = index(array_assign_lhs)
    { SafeExpr (Index index) }
  | e = array_assign_expr
    { SafeExpr e }

fn:
  | signature = signature; body = block
    { (signature, body) }
  ;

signature:
  | id = id; LPAREN; params = params; RPAREN; types = loption(types)
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
  | id = id; COLON; init = array_init
    { let (typ, es) = init in ArrayDecl (id, typ, es) }
  | id = id; GETS; e = enode
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
