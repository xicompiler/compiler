type nonrec 'a result = ('a, string) result
(** A [result] is either [Ok x] or [Error msg] where [msg] is a string
    detailing the error and [x] is the successful result of some
    calculation. *)

type 'a bind = Lexing.lexbuf -> 'a result
(** An ['a bind] maps a lexer buffer to an ['a result] *)

val bind : source:'a bind -> interface:'a bind -> string -> 'a result
(** Let [lexbuf] be a lexer buffer created from [file]. Then
    [bind ~source ~interface file] is [source lexbuf] if [file] is a xi
    source file, [interface lexbuf] if [file] is a xi interface file,
    and [Error] if [file] does not exist or is not a [Xi] file *)

val bind_same : f:'a bind -> string -> 'a result
(** Let [lexbuf] be a lexer buffer created from [file]. Then
    [bind_same ~f file] is [f lexbuf] if [file] is a xi source or
    interface file or [Error] if [file] does not exist or is not a [Xi]
    file *)
