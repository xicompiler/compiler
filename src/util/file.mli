open Core

val xi : string -> string
(** [xi s] concats ".xi" to [s] *)

val ixi : string -> string
(** [ixi s] concats ".ixi" to [s] *)

val is_xi : string -> bool
(** [is_xi s] is [true] iff [s] ends with [.xi] *)

val ixi_of_dir : dir:string -> string -> string
(** [ixi_of_dir ~dir file] is the string [dir/file.ixi] *)

val accessible : string -> bool
(** [accessible file] is [true] iff [Sys.file_exists file] is [`Yes] and
    [false] otherwise *)

val stdlib : string
(** [stdlib] is the path to the [Xi] standard library *)

val diagnostic : dir:string -> src:string -> string -> string
(** [diagnostic dir src ext] is the out path of the diagnostic file for
    [src] with extension [ext] in directory [dir] *)

val base : string -> string
(** [base file_path] is [file] where [file] is the basename of
    [file_path] without its extension *)

val println : string -> out:string -> unit
(** [println s ~out] prints string [s] to file [file] followed by a
    newline, truncating whatever was there before *)
