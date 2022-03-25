open OUnit2

val file_contents : string -> string
(** [file_contents in_file] is a string containing the contents of
    [in_file]. *)

val output_file : ?ext:string -> string -> string
(** [output_file ?ext src] is the test output file for [src] with
    extension [ext] *)

val map_file_tests :
  f:(string -> src:string -> reference:string -> test) ->
  string ->
  string ->
  test list
(** [map_file_tests ~f ref_ext dir] tests [f] on all files in [dir] with
    a xi or ixi extension, outputting files with extension [ref_ext] *)

val map_file_tests_xi :
  f:(string -> src:string -> reference:string -> test) ->
  string ->
  string ->
  test list
(** [map_file_tests_xi ~f ref_ext dir] tests [f] on all files in [dir]
    with a xi extension, outputting files with extension [ref_ext] *)

val map2_file_tests_xi :
  f:(string -> src:string -> reference:string -> test * test) ->
  string ->
  string ->
  test list
(** [map2_file_tests_xi ~f ref_ext dir] is [map_file_tests_xi] but with
    a function that returns two unit tests *)
