open Core

module type S = sig
  type t

  val to_string : t -> string
end
