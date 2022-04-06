type t = {
  line : int;
  column : int;
}
(** [t] is the type of position, consisting of the line and column *)

val format : t -> string -> string
(** [format pos s] is a function that formats a string [s] with [pos] *)

type position = t
(** [position] is an alias for [t] *)

(** [Error] represents an error with an associated position *)
module Error : sig
  type 'a t
  (** ['a t] is the type of an error with cause of type ['a], occurring
      at a specified position. *)

  val create : pos:position -> 'a -> 'a t
  (** [create ~pos cause] is a [t] ocurring at position [pos] with cause
      [cause] *)

  val cause : 'a t -> 'a
  (** [cause err] is the cause of [err] *)

  val position : 'a t -> position
  (** [position err] is the position at which [err] occurs *)

  val format : 'a t -> f:('a -> string) -> string
  (** [format err ~msg ~f] is a formatted error message [msg] using
      [f x], where [x] is the value wrapped in [err] *)

  val format_last :
    'a t ->
    f:('a -> string) ->
    fmt:('b -> string -> 'c, unit, string) format ->
    msg:'b ->
    'c
  (** TODO : remove this confusing function *)
end

type 'a error = 'a Error.t
(** [error] is an alias for [Error.t] *)

(** [Entry] is the type of an entry carrying data of type position *)
module Entry : sig
  type 'a t = ('a, position) Entry.t
  (** ['a t] represents an entry whose data is a [position] and whose
      key is type ['a] *)

  val error : cause:'err -> 'a t -> 'err error
  (** [error ~cause e] is an error with cause [cause] occurring at the
      position of [e] *)
end

type 'a entry = 'a Entry.t
(** ['a entry] is an alias for ['a Entry.t] *)

val of_lexer : Lexing.position -> t
(** [of_lexer pos] is the position [pos] *)

val of_lexbuf : Lexing.lexbuf -> t
(** [of_lexbuf lexbuf] is the current position of [lexbuf] *)
