open Core

type 'a t = 'a list
(** ['a t] is an alias for ['a list] *)

val fold2_result :
  unequal_lengths:'err ->
  f:('acc -> 'a -> 'b -> ('acc, 'err) result) ->
  init:'acc ->
  'a t ->
  'b t ->
  ('acc, 'err) result
(** fold2 ~unequal_lengths ~f ~init:a [b1; ...; bn] [c1; ...; cn] is
    [f (... (f (f a b1 c1) b2 c2) ...) bn cn], short circuiting on
    return of [Error _]. If the lengths of [l1] and [l2] are not equal,
    [Error unequal_lengths] is returned. *)

val hd_tl_exn : 'a t -> 'a * 'a t
(** [hd_tl_exn lst] is [h :: t] if [lst] is [h :: t]. Raises: [Failure]
    if [lst] is nil. *)

val rev_concat : 'a t t -> 'a t
(** Same as [Core.List.concat], but in reverse order *)

val rev_filter_opt : 'a option t -> 'a t
(** Same as [filter_opt], but reverses the list *)

val length : 'a t -> int64
(** [length lst] is [Int64.of_int (Core.List.length lst)] *)
