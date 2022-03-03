type t = {
  line : int;
  column : int;
}
(** [t] is the type of position, consisting of the line and column *)

type 'a error = {
  cause : 'a;
  position : t;
}
(** ['a error] is the type of an error with cause of type ['a],
    occurring at a specified position. *)

val get_position : Lexing.position -> t
(** [get_position pos] is the position [pos] *)

val get_position_lb : Lexing.lexbuf -> t
(** [get_position_lb lexbuf] is the current position of [lexbuf] *)
