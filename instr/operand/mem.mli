(** [Index] represents the index argument to a memory operand *)
module Index : sig
  type 'a t
  (** ['a t] represents an index of a memory operand, along with an
      optional scalar *)

  type scale =
    [ `One
    | `Two
    | `Four
    | `Eight
    ]
  (** [scale] is the type of a scalar multiplier used to scale the
      [scale] register *)

  val create : ?scale:scale -> 'a -> 'a t
  (** [create index ~scale] is an index argument of a memory operand *)
end

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

type 'a generic
(** ['a generic] represents a memory operand in Xi *)

val create :
  ?size:Size.t -> ?index:'a Index.t -> ?offset:int64 -> 'a -> 'a generic
(** [create base ~size ~index ~offset] is the memory operand [\size ptr
    [base + index * scale + offset\]]. If unspecified, [size] is
    [Qword]. *)

type t = Reg.t generic
(** [t] is the type of a memory operand for a concrete instruction *)

include Util.Stringable.S with type t := t

type abstract = Reg.abstract generic
(** [abstract] is the type of an abstract memory operand *)
