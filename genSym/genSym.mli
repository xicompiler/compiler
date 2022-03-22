type 'a t

val create :
  ?init:int ->
  (int -> 'a, unit, string, string, string, string) format6 ->
  'a t
(** [create init format] creates a symbol generator initialized with
    [init] and [format] *)

val generate : 'a t -> 'a
(** [generate gen] is a new symbol generated with [gen] *)
