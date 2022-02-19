val compile :
  ?lex:bool -> ?parse:bool -> string list -> (unit, string list) result
(** [compile ~lex ~parse files] compiles each of file of [files],
    performing lexing diagnostics iff [lex] is [true] and parsing
    diagnostics iff [parse] is true. If both [lex] and [parse] are
    false, their default values, then each provided file is compiled
    normally. [Ok ()] is yielded if no errors occur, and [Error es],
    where [es] is a list of error messages, is yielded if any errors
    occur. *)
