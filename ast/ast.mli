open Abstract

include
  S
    with module Expr.Node = Node.Ident
     and module Stmt.Node = Node.Ident
     and type integer := unit
     and type boolean := unit
     and type 'a vector := 'a

type nonrec expr = unit expr
(** [expr] is an alias for [unit expr] *)

val make_bop : Expr.binop -> expr -> expr -> expr
(** [make_bop bop e1 e2] is the binary operator expression [e1 bop e2] *)