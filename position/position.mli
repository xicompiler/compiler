type t = {
    line : int;
    column : int;
  }
(** [t] is the type of position, consisting of the line and column *)

val get_position : Lexing.position -> t
(** [get_position pos] is the position [pos] *)

val get_position_lb : Lexing.lexbuf -> t
(** [get_position_lb lexbuf] is the current position of [lexbuf] *)
