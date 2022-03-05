open Core

val fold2_result :
  unequal_lengths:'err ->
  f:('acc -> 'a -> 'b -> ('acc, 'err) result) ->
  init:'acc ->
  'a list ->
  'b list ->
  ('acc, 'err) result
(** fold2 ~unequal_lengths ~f ~init:a [b1; ...; bn] [c1; ...; cn] is
    [f (... (f (f a b1 c1) b2 c2) ...) bn cn], short circuiting on
    return of [Error _]. If the lengths of [l1] and [l2] are not equal,
    [Error unequal_lengths] is returned. *)
