open Core

let file_contents = In_channel.read_all

let output_file ?(ext = "output") src =
  let src_no_ext = Filename.chop_extension src in
  Printf.sprintf "%s.%s" src_no_ext ext

(** [make_test ~exts file f ref_ext dir] makes a test for [file] if
    [file] has an extension in [exts] *)
let make_test ~exts file f ref_ext dir =
  let ext = Caml.Filename.extension file in
  if List.exists ~f:(String.equal ext) exts then
    let name =
      file |> Filename.chop_extension |> Printf.sprintf "%s/%s" dir
    in
    let src = name ^ ext in
    let reference = name ^ ref_ext in
    Some (f name ~src ~reference)
  else None

let map_file_tests ~f ref_ext dir =
  let f file = make_test ~exts:[ ".xi"; ".ixi" ] file f ref_ext dir in
  Sys.readdir dir |> Array.to_list |> Stdlib.List.filter_map f

let map_file_tests_xi ~f ref_ext dir =
  let f file = make_test ~exts:[ ".xi" ] file f ref_ext dir in
  Sys.readdir dir |> Array.to_list |> Stdlib.List.filter_map f

let map2_file_tests_xi ~f ref_ext dir =
  let f file = make_test ~exts:[ ".xi" ] file f ref_ext dir in
  Sys.readdir dir |> Array.to_list
  |> Stdlib.List.filter_map f
  |> List.fold_left
       ~f:(fun acc (test1, test2) -> test1 :: test2 :: acc)
       ~init:[]
