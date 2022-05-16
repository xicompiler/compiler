type binop = Binop.t [@@deriving hash, compare, sexp]
(** [binop] is the type of a binary operator *)

(** [Binop] represents a binary operator*)
module Binop : module type of struct
  include Binop
end

type unop = Unop.t
(** [unop] is the type of a unary operator*)

(** [Unop] represents a unary operator *)
module Unop : module type of struct
  include Unop
end
