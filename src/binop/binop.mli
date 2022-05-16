open Core

type arith = Arith.t [@@deriving hash, compare, sexp]
(** [arith] is an alias for [Arith.t] *)

(** [Arith] represents an arithmetic binary operator *)
module Arith : module type of struct
  include Arith
end

type eq = Eq.t [@@deriving hash, compare, sexp]
(** [eq] is an alias for [Eq.t] *)

(** [Eq] represents the type of an equality binary operator *)
module Eq : module type of struct
  include Eq
end

type log = Log.t [@@deriving hash, compare, sexp]
(** [log] is an alias for [Log.t]*)

(** [Log] represents a logical binary operator on [bool]s *)
module Log : module type of struct
  include Log
end

type ord = Ord.t [@@deriving hash, compare, sexp]
(** [ord] is an alias for [Ord.t] *)

(** [Ord] represents a partial order on ints *)
module Ord : module type of struct
  include Ord
end

type cmp = Cmp.t [@@deriving hash, compare, sexp]
(** [cmp] is an alias for [Cmp.t] *)

(** [Cmp] represents a comparison operator on ints *)
module Cmp : module type of struct
  include Cmp
end

type t =
  [ arith
  | cmp
  | log
  ]
[@@deriving hash, compare, sexp]
(** A [t] represents a binary operator *)

val to_string : [< t ] -> string
(** [to_string bop] is the string representation of [bop] *)

val eval_primitive :
  [< t ] ->
  [< Primitive.t ] ->
  [< Primitive.t ] ->
  Primitive.base option
(** [eval_primitive op x1 x2] is [Some (x1 op x2)] if the types of the
    operator and the operands match and the operation is sucessful, or
    [None] otherwise *)

val eval_array : [< t ] -> 'a list -> 'a list -> 'a list option
(** [eval_array op a1 a2] is [Some (a1 op a2)] if the types of the
    operator and the operands match and the operation is sucessful, or
    [None] otherwise *)
