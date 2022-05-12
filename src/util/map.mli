open Core

type ('k, 'v, 'cmp) t = ('k, 'v, 'cmp) Map.t
(** An alias for [Map.t] *)

val update_exn :
  ?message:string ->
  ('k, 'v, 'cmp) t ->
  'k ->
  f:('v -> 'v) ->
  ('k, 'v, 'cmp) t
(** Same as [Map.update], but raises if the provdided key is unbound
    with an optional error message *)

val pop_elt : ('k, 'v, 'cmp) t -> ('k * 'v * ('k, 'v, 'cmp) t) option
(** [pop_elt m] is [Some (k, v, m')] where [k] is bound to [v] in [m]
    and [m'] is [m] without a binding for [k], or [None] if [m] is empty *)
