type t = int64 [@@deriving sexp]
(** [t] is the type of an immediate operand *)

val of_int : int -> [> `Imm of int64 ]
(** [of_int i] is [`Imm i'] where [i'] is the integer representation of
    [i] *)
