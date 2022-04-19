open Core

(** [Index] represents the index argument to a memory operand *)
module Index : sig
  type 'a t
  (** ['a t] represents an index of a memory operand, along with an
      optional scalar *)

  (** [Scale] represents a scalar to the index register *)
  module Scale : sig
    type t = int64
    (** A [t] is an [int64] *)

    val is_valid : t -> bool
    (** [is_valid scale] is [true] iff [scale] is 1, 2, 4, or 8 *)
  end

  val create : ?scale:Scale.t -> 'a -> 'a t
  (** [create index ~scale] is an index argument of a memory operand.
      Requires: [Scale.is_valid scale] *)

  val index : 'a t -> 'a
  (** [index idx] is the index register of [idx] *)

  val with_index : 'a t -> index:'b -> 'b t
  (** [with_index idx ~index] is [idx] with its index register set to
      [index], leaving its scale unchanged *)
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
  ?segment:Ir.label ->
  ?size:Size.t ->
  ?index:'a Index.t ->
  ?offset:int64 ->
  'a ->
  'a generic
(** [create base ~segment ~size ~index ~offset] is the memory operand
    [\size ptr segment[base + index * scale + offset\]]. If unspecified,
    [size] is [Qword]. *)

type t = Reg.t generic
(** [t] is the type of a memory operand for a concrete instruction *)

module Abstract : sig
  type t = Reg.Abstract.t generic
  (** [abstract] is the type of an abstract memory operand *)

  include Util.Stringable.S with type t := t
end

val base : 'a generic -> 'a
(** [base mem] is the base register of memory operand [mem] *)

val index : 'a generic -> 'a Index.t option
(** [index mem] is [Some index] where [index] is the index register and
    scale of memory operand [mem] if present, or [None] if there is no
    index operand *)

val with_registers :
  ?index:'b Index.t -> base:'b -> 'a generic -> 'b generic
(** [with_registers ~index ~base mem] is [mem] with its index and base
    registers updated to [index] and [base], respectively *)

include Util.Stringable.S with type t := t
