val compile :
  ?lex:bool ->
  ?parse:bool ->
  ?src_dir:string ->
  ?out_dir:string ->
  string list ->
  (unit, string list) result
(** [compile ~lex ~parse ~src_dir ~out_dir files] compiles each of file
    of [files], performing lexing diagnostics iff [lex] is [true] and
    parsing diagnostics iff [parse] is true from output directory
    [src_dir] to output directory [out_dir] both of which are the
    current directory if not provided. If both [lex] and [parse] are
    false, their default values, then each provided file is compiled
    normally. [Ok ()] is yielded if no errors occur, and [Error es],
    where [es] is a list of error messages, is yielded if any errors
    occur. *)
