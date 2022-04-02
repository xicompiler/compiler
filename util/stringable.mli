open Core

(** [S] is the abstract module type of a module with a type [t]
    convertable to a string *)
module type S = sig
  type t
  (** [t] is a type convertable to a string *)

  val to_string : t -> string
  (** [to_string t] is the string representation of [t] *)
end
