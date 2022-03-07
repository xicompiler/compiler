open! Core

val xi : string -> string
(** [xi s] concats ".xi" to [s] *)

val ixi : string -> string
(** [ixi s] concats ".ixi" to [s] *)

val ixi_of_dir : dir:string -> string -> string
(** [ixi_of_dir ~dir file] is the string [dir/file.ixi] *)

val accessible : string -> bool
(** [accessible file] is [true] iff [Sys.file_exists file] is [`Yes] and
    [false] otherwise *)

val stdlib : string
(** [stdlib] is the path to the [Xi] standard library *)

val diagnostic : string -> string -> file:string -> string
(** [diagnostic ext dir file] is the out path of the diagnostic file for
    [file] with extension [ext] in direction [dir] *)
