%{
  open Core
  open Ast
  open Undecorated
  open Expr
  open Stmt
  open Toplevel

  let node ~pos t = Entry.create ~key:t ~data:(Position.of_lexer pos)

  let int_err pos i =
    let pos = Position.of_lexer pos in
    let err = Position.Error.create ~pos i in
    raise (Exception.InvalidIntLiteral err)

  type unsafe_int =
    | SafeInt of int64
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
    | UnsafeInt i -> `Int (int_of_unsafe ~pos i)

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
%token BREAK
%token NULL
%token RECORD

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

(* Record field access operator, [.] *)
%token DOT

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

%start <Undecorated.t> rho_prog
%start <Undecorated.source> rho_source
%start <Undecorated.intf> rho_intf
%start <Undecorated.t> xi_prog
%start <Undecorated.source> xi_source
%start <Undecorated.intf> xi_intf

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
rho_enode:
  | node = node(rho_expr)
    { node }
  ;

xi_enode:
  | node = node(xi_expr)
    { node }
  ;

(** [enodes] produces a comma-separated list of [enode]s *)
rho_enodes:
  | nodes = separated_list(COMMA, rho_enode)
    { nodes }
  ;

xi_enodes:
  | nodes = separated_list(COMMA, xi_enode)
    { nodes }
  ;

(** [snode] produces a node wrapping a statement *)
rho_snode:
  | node = node(rho_stmt)
    { node }
  ;

xi_snode:
  | node = node(xi_stmt)
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
  | MULT { `Mul }
  | HIGHMULT { `HMul }
  | DIV { `Div }
  | MOD { `Mod }
  | PLUS { `Add }
  | MINUS { `Sub }
  | LT { `Lt }
  | LEQ { `Leq}
  | GEQ { `Geq }
  | GT { `Gt }
  | EQ { `Eq }
  | NEQ { `Neq }
  | AND { `And }
  | OR { `Or }
  ;

(** A [unop] is a unary operator in Xi *)
%inline unop:
  | MINUS { `IntNeg }
  | NOT { `LogNeg }
  ;

(** A prog is either a source or intf *)
rho_prog:
  | s = rho_source { Source s }
  | i = rho_intf { Intf i }
  ;

xi_prog:
  | s = xi_source {Source s}
  | i = xi_intf {Intf i}

(** A [source] derives a source file in  Xi, followed by EOF *)
rho_source:
  | s = rho_source_file; EOF
    { s }
  ;

xi_source:
  | s = xi_source_file; EOF 
    { s }
  ;

(** An [intf] derives an intf file in Xi, followed by EOF  *)
rho_intf:
  | sigs = node(rho_intf_sig)+; EOF
    { {uses=[]; sigs} }
  | uses = node(use)+; sigs = node(rho_intf_sig)+; EOF
    { {uses; sigs} }
  ;

xi_intf:
  | sigs = node(xi_intf_sig)+; EOF
    { {uses=[]; sigs} }
  ;

rho_intf_sig:
  | defn = rho_signature
    { FnSig defn }
  | record = record
    { let (name, vars) = record in RecordSig (name, vars) }
  ;

xi_intf_sig:
  | defn = xi_signature
    { FnSig defn }
  ;

record:
  | RECORD; name = id; LBRACE; vars = list(rho_decl) RBRACE
    { (name, vars) }

(** A [source_file] derives a source file in  Xi *)
rho_source_file:
  | defs = rho_definitions
    { Source.create ~uses:[] ~defs }
  | uses = node(use)+; defs = rho_definitions
    { Source.create ~uses ~defs  }
  ;

xi_source_file:
  | defs = xi_definitions
    { Source.create ~uses:[] ~defs }
  | uses = node(use)+; defs = xi_definitions
    { Source.create ~uses ~defs  }
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

rho_definitions:
  | global = node(rho_global_semi); definitions = rho_definitions
    { global :: definitions }
  | fn_defn = node(rho_fn_defn); definitions = node(rho_definition)*
    { fn_defn :: definitions }
  | record_defn = node(record_defn); definitions = node(rho_definition)*
    { record_defn :: definitions }
  ;

xi_definitions:
  | global = node(xi_global_semi); definitions = xi_definitions
    { global :: definitions }
  | fn_defn = node(xi_fn_defn); definitions = node(xi_definition)*
    { fn_defn :: definitions }
  ;

rho_definition:
  | defn = rho_global_semi
  | defn = rho_fn_defn
    { defn }
  | defn = record_defn
    { defn }
  ;

xi_definition:
  | defn = xi_global_semi
  | defn = xi_fn_defn
    { defn }
  ;

rho_global_semi:
  | global = semi(rho_global)
    { global }
  ;

xi_global_semi:
  | global = semi(xi_global)
    { global }
  ;

rho_fn_defn:
  | fn = rho_fn
    {  FnDefn fn }
  ;

xi_fn_defn:
  | fn = xi_fn
    {  FnDefn fn }
  ;

record_defn:
  | record = record
    { let (name, vars) = record in RecordDefn (name, vars) }
  ;

rho_global:
  | decl = rho_decl
    { GlobalDecl decl }
  | id = id; COLON; typ = rho_typ; GETS; global_rhs = global_rhs
    {
      GlobalInit (id, typ, global_rhs)
    }
  ;

xi_global:
  | decl = xi_decl
    { GlobalDecl decl }
  | id = id; COLON; typ = xi_typ; GETS; global_rhs = global_rhs
    {
      GlobalInit (id, typ, global_rhs)
    }
  ;

global_rhs:
  | v = primitive 
    {
      primitive_of_unsafe ~pos:$startpos(v) v
    }
  | neg = MINUS; i = INT 
    {
      let pos = $startpos(neg) in
      let i = parse_int ~pos ~neg:true i in
      `Int (int_of_unsafe ~pos i)
    }
  ;

rho_decl:
  | ids = separated_nonempty_list(COMMA, id); COLON; typ = rho_typ
    { (ids, typ) }
  | id = id; COLON; typ = rho_typ
    { ([id], typ) }
  ;

xi_decl:
  | id = id; COLON; typ = xi_typ
    { ([id], typ) }
  ;

rho_typ:
  | typ = TYPE
    { typ :> Type.tau }
  | typ = rho_typ; LBRACKET; RBRACKET
    { `Array typ }
  | typ = ID
    { `Record typ }
  ;

xi_typ:
| typ = TYPE
  { typ :> Type.tau }
| typ = xi_typ; LBRACKET; RBRACKET
  { `Array typ }
;

rho_array_init:
  | base = rho_typ; e = bracketed(rho_enode); es = bracketed(rho_enode?)*
    { 
      let es = Some e :: es in
      (List.fold ~f:(fun acc _ -> `Array acc) ~init:base es, es)
    }
  ;

xi_array_init:
  | base = xi_typ; e = bracketed(xi_enode); es = bracketed(xi_enode?)*
    { 
      let es = Some e :: es in
      (List.fold ~f:(fun acc _ -> `Array acc) ~init:base es, es)
    }
  ;

rho_index(lhs):
  | e1 = lhs; e2 = bracketed(rho_enode)
    { (enode_of_unsafe ~pos:$startpos e1, e2) }
  ;

xi_index(lhs):
  | e1 = lhs; e2 = bracketed(xi_enode)
    { (enode_of_unsafe ~pos:$startpos e1, e2) }
  ;

field(lhs):
  | e1 = lhs; DOT; id = id
    { (enode_of_unsafe ~pos:$startpos e1, id) }

rho_expr:
  | e = rho_bop_expr
    { expr_of_unsafe ~pos:$startpos(e) e }
  ;

xi_expr:
  | e = xi_bop_expr
    { expr_of_unsafe ~pos:$startpos(e) e }
  ;

rho_bop_expr:
  | e1 = rho_enode; bop = binop; e2 = rho_enode
    { SafeExpr (Bop (bop, e1, e2)) }
  | e = rho_uop_expr
    { e }
  ;

xi_bop_expr:
  | e1 = xi_enode; bop = binop; e2 = xi_enode
    { SafeExpr (Bop (bop, e1, e2)) }
  | e = xi_uop_expr
    { e }
  ;

rho_uop_expr:
  | uop = unop; e = rho_uop_expr
    {
      let pos = $startpos(e) in
      match uop, e with
      | `IntNeg, SafeExpr (Primitive (`Int i)) ->
        if Int64.is_negative i then
         SafeExpr (Uop (uop, (enode_of_unsafe ~pos e)))
        else
          UnsafePrimitive (UnsafeInt (SafeInt (Int64.neg i)))
      | `IntNeg, UnsafePrimitive (UnsafeInt (IntBound _)) ->
          UnsafePrimitive (UnsafeInt (SafeInt (Int64.min_value)))
      | _ -> 
         SafeExpr (Uop (uop, (enode_of_unsafe ~pos e)))
    }
  | e = rho_call_expr
    { e }
  ;

xi_uop_expr:
  | uop = unop; e = xi_uop_expr
    {
      let pos = $startpos(e) in
      match uop, e with
      | `IntNeg, SafeExpr (Primitive (`Int i)) ->
        if Int64.is_negative i then
         SafeExpr (Uop (uop, (enode_of_unsafe ~pos e)))
        else
          UnsafePrimitive (UnsafeInt (SafeInt (Int64.neg i)))
      | `IntNeg, UnsafePrimitive (UnsafeInt (IntBound _)) ->
          UnsafePrimitive (UnsafeInt (SafeInt (Int64.min_value)))
      | _ -> 
         SafeExpr (Uop (uop, (enode_of_unsafe ~pos e)))
    }
  | e = xi_call_expr
    { e }
  ;

rho_call_expr:
  | index = rho_index(rho_call_expr)
    { let (e1, e2) = index in SafeExpr (Index (e1, e2)) }
  | field = field(rho_call_expr)
    { let (e1, id) = field in SafeExpr (Field (e1, id)) }
  | e = parens(rho_expr)
    { SafeExpr e }
  | v = primitive
    { UnsafePrimitive v }
  | LBRACE; array = rho_array
    { SafeExpr (Array array) }
  | s = STRING
    { SafeExpr (String s) }
  | NULL
    { SafeExpr (Null) }
  | LENGTH; e = parens(rho_enode)
    { SafeExpr (Length e) }
  | e = rho_assign_expr
    { SafeExpr e }
  ;

xi_call_expr:
  | index = xi_index(xi_call_expr)
    { let (e1, e2) = index in SafeExpr (Index (e1, e2)) }
  | e = parens(xi_expr)
    { SafeExpr e }
  | v = primitive
    { UnsafePrimitive v }
  | LBRACE; array = xi_array
    { SafeExpr (Array array) }
  | s = STRING
    { SafeExpr (String s) }
  | LENGTH; e = parens(xi_enode)
    { SafeExpr (Length e) }
  | e = xi_assign_expr
    { SafeExpr e }
  ;

primitive:
  | i = unsafe_int
    { UnsafeInt i }
  | b = BOOL
    { SafePrimitive (`Bool b) }
  | c = CHAR
    { SafePrimitive (`Char c) }
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

rho_array:
  | e = rho_enode?; RBRACE
    { Option.to_list e }
  | e = rho_enode; COMMA; rest = rho_array
    { e :: rest }
  ;

xi_array:
  | e = xi_enode?; RBRACE
    { Option.to_list e }
  | e = xi_enode; COMMA; rest = xi_array
    { e :: rest }
  ;

rho_call:
  | id = id; args = parens(rho_enodes)
    { (id, args) }
  ;

xi_call:
  | id = id; args = parens(xi_enodes)
    { (id, args) }
  ;

(** [assign_expr] is any non-array-index expression [e1] that can appear 
    in the statement [e1[e2] = e3] *)
rho_assign_expr:
  | call = rho_call
    { FnCall call }
  | id = id
    { Id id }
  ;

xi_assign_expr:
  | call = xi_call
    { FnCall call }
  | id = id
    { Id id }
  ;

(** [array_assign_lhs] is any expression e1 that can appear in the statement 
    [e1[e2] = e3] *)
rho_array_assign_lhs:
  | index = rho_index(rho_array_assign_lhs)
    { let (e1, e2) = index in SafeExpr (Index (e1, e2)) }
  | e = rho_assign_expr
    { SafeExpr e }

xi_array_assign_lhs:
  | index = xi_index(xi_array_assign_lhs)
    { let (e1, e2) = index in SafeExpr (Index (e1, e2)) }
  | e = xi_assign_expr
    { SafeExpr e }

field_assign_lhs:
  | field = field(field_assign_lhs)
    { let (e1, id) = field in SafeExpr (Field (e1, id)) }
  | e = rho_assign_expr
    { SafeExpr e }

rho_fn:
  | signature = rho_signature; body = rho_block
    { (signature, body) }
  ;

xi_fn:
  | signature = xi_signature; body = xi_block
    { (signature, body) }
  ;

rho_signature:
  | name = id; LPAREN; params = rho_params; RPAREN; ret = loption(rho_types)
    { Sig.create ~name ~params ~ret }
  ;

xi_signature:
  | name = id; LPAREN; params = xi_params; RPAREN; ret = loption(xi_types)
    { Sig.create ~name ~params ~ret }
  ;

rho_params:
  | params = separated_list(COMMA, rho_param)
    { params }
  ;

xi_params:
  | params = separated_list(COMMA, xi_param)
    { params }
  ;

rho_param:
  | id = id; COLON; typ = rho_typ
    { (id, typ) }

xi_param:
  | id = id; COLON; typ = xi_typ
    { (id, typ) }

rho_types:
  | COLON; types = separated_nonempty_list(COMMA, rho_typ)
    { types }
  ;

xi_types:
  | COLON; types = separated_nonempty_list(COMMA, xi_typ)
    { types }
  ;

rho_block:
  | LBRACE; body = list_maybe_followed(rho_snode, node(rho_return)); RBRACE
    { body }
  ;

xi_block:
  | LBRACE; body = list_maybe_followed(xi_snode, node(xi_return)); RBRACE
    { body }
  ;

rho_return:
  | RETURN; es = semi(rho_enodes);
    { Return es }
  ;

xi_return:
  | RETURN; es = semi(xi_enodes);
    { Return es }
  ;

rho_stmt:
  | stmt = rho_if_stmt
  | stmt = rho_if_else
  | stmt = rho_while_stmt
  | stmt = break
  | stmt = semi(rho_semicolon_terminated)
    { stmt }
  ;

xi_stmt:
  | stmt = xi_if_stmt
  | stmt = xi_if_else
  | stmt = xi_while_stmt
  | stmt = semi(xi_semicolon_terminated)
    { stmt }
  ;

break:
  | BREAK;
    { Break }

rho_if_stmt:
  | IF; e = rho_enode; s = rho_snode
    { If (e, s) }
  ;

xi_if_stmt:
  | IF; e = xi_enode; s = xi_snode
    { If (e, s) }
  ;

rho_if_else:
  | IF; e = rho_enode; s1 = rho_snode; ELSE; s2 = rho_snode
    { IfElse (e, s1, s2) }
  ;

xi_if_else:
  | IF; e = xi_enode; s1 = xi_snode; ELSE; s2 = xi_snode
    { IfElse (e, s1, s2) }
  ;

rho_while_stmt:
  | WHILE; e = rho_enode; stmt = rho_snode
    { While (e, stmt) }
  ;

xi_while_stmt:
  | WHILE; e = xi_enode; stmt = xi_snode
    { While (e, stmt) }
  ;

separated_multiple_list(sep, X):
  | x = X; sep; xs = separated_nonempty_list(sep, X)
    { x :: xs }

rho_semicolon_terminated:
  | decl = rho_decl
    { VarDecl decl }
  | id = id; COLON; typ = rho_typ; GETS; e = rho_enode
    { VarInit (id, typ, e) }
  | id = id; COLON; init = rho_array_init
    { let (typ, es) = init in ArrayDecl (id, typ, es) }
  | id = id; GETS; e = rho_enode
    { Assign (id, e) }
  | index = rho_index(rho_array_assign_lhs); GETS; e3 = rho_enode
    { let (e1, e2) = index in ArrAssign (e1, e2, e3) }
  | field = field(field_assign_lhs); GETS; e3 = rho_enode
    { let (e1, id) = field in FieldAssign (e1, id, e3) }
  | ds = separated_multiple_list(COMMA, rho_d); GETS; rhs = rho_call
    { let (id, args) = rhs in MultiAssign (ds, id, args) }
  | WILDCARD; GETS; rhs = rho_call
    { ExprStmt rhs }
  | call = rho_call
    { PrCall call }
  | block = rho_block
    { Block block }
  ;

xi_semicolon_terminated:
  | decl = xi_decl
    { VarDecl decl }
  | id = id; COLON; typ = xi_typ; GETS; e = xi_enode
    { VarInit (id, typ, e) }
  | id = id; COLON; init = xi_array_init
    { let (typ, es) = init in ArrayDecl (id, typ, es) }
  | id = id; GETS; e = xi_enode
    { Assign (id, e) }
  | index = xi_index(xi_array_assign_lhs); GETS; e3 = xi_enode
    { let (e1, e2) = index in ArrAssign (e1, e2, e3) }
  | ds = separated_multiple_list(COMMA, xi_d); GETS; rhs = xi_call
    { let (id, args) = rhs in MultiAssign (ds, id, args) }
  | WILDCARD; GETS; rhs = xi_call
    { ExprStmt rhs }
  | call = xi_call
    { PrCall call }
  | block = xi_block
    { Block block }
  ;

rho_d:
  | id = id; COLON; typ = rho_typ 
    { Some (id, typ) }
  | WILDCARD
    { None }
  ;
  
xi_d:
  | id = id; COLON; typ = xi_typ 
    { Some (id, typ) }
  | WILDCARD
    { None }
  ;


