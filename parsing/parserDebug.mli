val parse_to_file : src:string -> dst:string -> unit
(** [parse_to_file ~src ~dst] parses the file at path [src] and writes
    the results to the file at path [dst], serialized in an
    S-expression. *)
