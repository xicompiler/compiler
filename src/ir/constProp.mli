open Core

type defined =
  | Top
  | Def of int64
  | Bottom
[@@deriving hash, compare, sexp, equal]

module Analysis : sig
  val params :
    gen:
      (defined Temp.Virtual.Map.t ->
      'a ->
      (Temp.Virtual.t * defined) option) ->
    stmts:Lir.stmt list ->
    (defined Temp.Virtual.Map.t, 'a) Dataflow.Params.t
  (** [params] is the dataflow parameters required for constant
      propagation analysis *)
end

val propagate : Lir.t -> Lir.t
(** [propagate lir] is [lir] with constant propagation performed *)
