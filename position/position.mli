type t = {
  line : int;
  column : int;
}
(** [t] is the type of position, consisting of the line and column *)

type position = t
(** [position] is an alias for [t] *)

(** [Error] represents an error with an associated position *)
module Error : sig
  type 'a t
  (** ['a t] is the type of an error with cause of type ['a], occurring
      at a specified position. *)

  val make : pos:position -> 'a -> 'a t
  (** [make ~pos cause] is a [t] ocurring at position [pos] with cause
      [cause] *)

  val cause : 'a t -> 'a
  (** [cause err] is the cause of [err] *)

  val position : 'a t -> position
  (** [position err] is the position at which [err] occurs *)
end

type 'a error = 'a Error.t
(** [error] is an alias for [Error.t] *)

val get_position : Lexing.position -> t
(** [get_position pos] is the position [pos] *)

val get_position_lb : Lexing.lexbuf -> t
(** [get_position_lb lexbuf] is the current position of [lexbuf] *)
