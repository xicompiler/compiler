open Core

module Analysis : sig
  val params :
    gen:('a -> (Temp.Virtual.t * Temp.Virtual.t) option) ->
    kill:('a -> Temp.Virtual.Set.t) ->
    stmts:Lir.stmt list ->
    (Temp.Virtual.Set.t Temp.Virtual.Map.t, 'a) Dataflow.Params.t
  (** [params] is the dataflow parameters required for copy propagation
      analysis *)
end

val propagate : Lir.t -> Lir.t
(** [propagate lir] is [lir] with copy propagation performed *)
