open Core

(** [Error] represents the errors that can occur from binding a function
    to a Xi file *)
module Error : sig
  type t =
    [ `NoSuchFile of string
    | `NotXiFile of string
    ]
  [@@deriving variants]
  (** An [error] represents an error that can occur while performing an
      operation on a [Xi] file *)

  val not_xi_file : string -> string
  (** [not_xi_file s] is "s: Not a Xi file" *)

  val to_string : t -> string
  (** [to_string err] is the string representing [err] *)
end

type error = Error.t
(** [error] is an alias for [Error.t] *)

type nonrec 'a result = ('a, error) result
(** A [result] is either [Ok] of ['a] or [Error] *)

type 'a map = Lexing.lexbuf -> 'a
(** An ['a map] maps terms of type [Lexing.lexbuf] to terms of type ['a] *)

val map :
  source:'a map -> intf:'b map -> string -> ('a, 'b) Either.t result
(** Let [lexbuf] be a lexer buffer created from [file]. Then
    [map ~source ~intf file] is [Ok (First (source lexbuf))] if [file]
    is a xi source file, [Ok (Second (intf lexbuf))] if [file] is a xi
    intf file, and [Error] if [file] does not exist or is not a [Xi]
    file *)

val map_same : source:'a map -> intf:'a map -> string -> 'a result
(** Let [lexbuf] be a lexer buffer created from [file]. Then
    [map ~source ~intf file] is [Ok (source lexbuf)] if [file] is a xi
    source file, [Ok (intf lexbuf)] if [file] is a xi intf file, and
    [Error] if [file] does not exist or is not a [Xi] file *)

val map_same_fn : f:'a map -> string -> 'a result
(** Let [lexbuf] be a lexer buffer created from [file]. Then
    [map_same_fn ~f file] is [Ok (f lexbuf)] if [file] is a Xi source or
    intf file and [Error] if [file] does not exist or is not a [Xi] file *)
