type nonrec 'a file_result = ('a, string) result
(** A [result] is either [Ok x] or [Error msg] where [msg] is a string
    detailing the error and [x] is the successful result of some
    calculation. *)

type 'a apply = Lexing.lexbuf -> 'a file_result
(** [apply] is the type of a function that can be applied to a lexer
    buffer and yields a [result]. *)

val bind :
  source:'a apply -> interface:'a apply -> string -> 'a file_result
(** Let [lexbuf] be a lexer buffer created from [file]. Then
    [bind ~source ~interface file] is [source lexbuf] if [file] is a xi
    source file, [interface file] if [file] is a xi interface file, and
    [Error] if [file] does not exist. *)

val iter_all :
  source:unit apply ->
  interface:unit apply ->
  string list ->
  (unit, string list) result
(** [iter_all ~source ~dst \[f1;...; fn\]] is [Ok ()] if each of [bind
    ~source ~inteface f1, ..., bind ~source ~interface fn] are [Ok ()],
    or is [Error errs]] where [errs] is a list of all error messages
    generated during each application of [bind] *)
