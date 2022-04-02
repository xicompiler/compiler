(** [Index] represents the index argument to a memory operand *)
module Index : sig
  type t
  (** [t] represents an index of a memory operand, along with an
      optional scalar *)

  type scale =
    [ `One
    | `Two
    | `Four
    | `Eight
    ]
  (** [scale] is the type of a scalar multiplier used to scale the
      [scale] register *)

  val make : ?scale:scale -> Reg.t -> t
  (** make [make index ~scale] is an index argument of a memory operand *)

  include Util.Stringable.S with type t := t
end

type t
(** [t] represents a memory operand in Xi *)

(** [Size] is the size specifier for a size directive. *)
module Size : sig
  (** [t] is the size specifier for a size directive. *)
  type t =
    | Qword
    | Dword
    | Word
    | Byte

  include Util.Stringable.S with type t := t
end

val make : ?size:Size.t -> ?index:Index.t -> ?offset:int64 -> Reg.t -> t
(** [make base ~size ~index ~offset] is the memory operand [\size ptr
    [base + index * scale + offset\]]. If unspecified, [size] is
    [Qword]. *)

include Util.Stringable.S with type t := t
