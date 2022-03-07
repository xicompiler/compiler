open OUnit2

val file_contents : string -> string
(** [file_contents in_file] is a string containing the contents of
    [in_file]. *)

val map_file_tests :
  (string -> src:string -> out:string -> reference:string -> test) ->
  string ->
  string ->
  test list
(** [map_file_tests f ref_ext dir] tests [f] on all files in [dir],
    outputting files with extension [ref_ext] *)
