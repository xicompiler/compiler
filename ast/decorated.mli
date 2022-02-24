open Types

include
  Abstract.S
    with module Expr.Node = Node.Ident
     and module Stmt.Node = Node.Ident
     and type integer := Typed.integer
     and type boolean := Typed.boolean
     and type 'a vector := 'a Typed.vector
