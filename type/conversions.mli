open Core
open Definitions

val tau_of_expr : expr -> tau option
(** [tau_of_expr e] is [Some t] if [e] is tau type [t] and [None]
    otherwise *)

val tau_of_expr_res :
  expr -> Position.t -> tau TypeError.Positioned.result
(** [tau_of_expr_res e] is [Ok t] if [e] is tau type [t] and a type
    error otherwise *)

val tau_list_of_term : [< term ] -> tau list
(** [tau_list_of_term e] is [e] converted from a [term] to a [tau list] *)

val term_of_tau_list : tau list -> term
(** [term_of_tau_list e] is [e] converted from a [tau list] to a [term] *)

val expr_of_term : [< term ] -> expr
(** [expr_of_term e] is [e] converted from a [term] to an [expr] type *)
