open Core

(** [Make (Set)] contains functions for computing liveness, where each
    set of variables is represented as a [Set.t] *)
module Make (Set : Set.S) : sig
  val params :
    use:('v -> Set.t) -> def:('v -> Set.t) -> (Set.t, 'v) Params.t
  (** [params ~use ~def] are the parameters of a dataflow analysis for
      liveness, wehre [use v] is the set of variables used by [v] and
      [def v] is the set of variables defined by [v] *)
end