val printable_ascii_min : char
(** [printable_ascii_min] is the minimum printable ascii value,
    inclusive. *)

val printable_ascii_max : char
(** [printable_ascii_max] is the maximum printable ascii value,
    inclusive. *)

val iter : (Uchar.t -> unit) -> string -> unit
(** [iter f s] applies [f] to each unicode codepoint of [s]. Raises:
    [Invalid_argument] if [s] is malformed. *)

val fold : ('a -> Uchar.t -> 'a) -> 'a -> string -> 'a
(** [fold f init s] applies a left fold over [s] with [f], using [init]
    as the initial value of the accumulator. Raises: [Invalid_argument]
    if [s] is malformed. *)

val uchars_of_string : string -> Uchar.t Seq.t
(** [uchars_of_string s] is a sequence containing every codepoint of [s] *)

val string_of_uchar : Uchar.t -> string
(** [string_of_uchar u] is the escaped string representing unicode
    character [u] *)

val escape_string : string -> string
(** [escape_string s] is [s] escaped and properly formatted to be
    printed according to Xi conventions. *)
