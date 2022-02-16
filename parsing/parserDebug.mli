val parse_to_file : src:string -> dst:string -> (unit, string) result
(** [parse_to_file ~src ~dst] parses the file at path [src] and writes
    the results to the file at path [dst], serialized in an
    S-expression. The application is [Ok ()] if no error ocuurs, or
    [Error msg] if a lexical or syntax error occurs where [msg] details
    the message. *)
