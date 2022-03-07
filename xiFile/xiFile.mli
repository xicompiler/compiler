type nonrec 'a result = ('a, string) result
(** A [result] is either [Ok x] or [Error msg] where [msg] is a string
    detailing the error and [x] is the successful result of some
    calculation. *)

type 'a bind = Lexing.lexbuf -> 'a result
(** An ['a bind] maps a lexer buffer to an ['a result] *)

val bind : source:'a bind -> intf:'a bind -> string -> 'a result
(** Let [lexbuf] be a lexer buffer created from [file]. Then
    [bind ~source ~intf file] is [source lexbuf] if [file] is a xi
    source file, [intf lexbuf] if [file] is a xi intf file, and [Error]
    if [file] does not exist or is not a [Xi] file *)

type 'a map = Lexing.lexbuf -> 'a
(** An ['a map] maps terms of type [Lexing.lexbuf] to terms of type ['a] *)

val map : source:'a map -> intf:'a map -> string -> 'a result
(** Let [lexbuf] be a lexer buffer created from [file]. Then
    [map ~source ~intf file] is [Ok (source lexbuf)] if [file] is a xi
    source file, [Ok (intf lexbuf)] if [file] is a xi intf file, and
    [Error] if [file] does not exist or is not a [Xi] file *)

val map_same : f:'a map -> string -> 'a result
(** Let [lexbuf] be a lexer buffer created from [file]. Then
    [map_same ~f file] is [Ok (f lexbuf)] if [file] is a Xi source or
    intf file and [Error] if [file] does not exist or is not a [Xi] file *)
