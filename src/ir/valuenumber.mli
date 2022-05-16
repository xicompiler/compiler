module Expr : sig
  type t =
    | Bop of Op.t * int * int
    | Temp of string
    | Const of int64
  [@@deriving hash, compare, sexp]
end

val number_stmt :
  (Expr.t, int) Core.Hashtbl.t ->
  Reorder.stmt ->
  (Expr.t, int) Core.Hashtbl.t

val number_stmts : Reorder.stmt list -> (Expr.t, int) Core.Hashtbl.t
