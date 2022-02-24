(** [S] describes the abstract types of expressions found within an AST *)
module type S = sig
  type integer
  (** [integer] is the abstract type of an int *)

  type boolean
  (** [boolean] is the abstract type of a bool *)

  type !'a vector
  (** [vector] is the abstract type of an array*)
end

(** [Typed] is a module containing statically checked types *)
module Typed :
  S
    with type integer = int
     and type boolean = bool
     and type 'a vector = 'a array

(** [Untyped] is a module describing an untyped system *)
module Untyped :
  S
    with type integer = unit
     and type boolean = unit
     and type 'a vector = 'a
