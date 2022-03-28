val no_such_file : string -> string
(** [no_such_file s] is "s: No such file" *)

type error = string
(** [error] is the type of a file error, the name of the unbound file *)

type nonrec 'a result = ('a, error) result
(** A [result] is either [Ok] of ['a] or [Error] *)

val map : f:(Lexing.lexbuf -> 'a) -> string -> 'a result
(** Let [lexbuf] be a lexer buffer created from [file]. Then
    [map ~source ~intf file] is [Ok (source lexbuf)] if [file] is a xi
    source file, [Ok (intf lexbuf)] if [file] is a xi intf file, and
    [Error] if [file] does not exist or is not a [Xi] file *)
