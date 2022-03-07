let file_contents in_file =
  let ch = open_in in_file in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let map_file_tests f ref_ext dir =
  let make_test file =
    let ext = Filename.extension file in
    if ext = ".xi" || ext = ".ixi" then
      let name =
        file |> Filename.remove_extension |> Printf.sprintf "%s/%s" dir
      in
      let src = name ^ ext in
      let out = name ^ ".output" in
      let reference = name ^ ref_ext in
      Some (f name ~src ~out ~reference)
    else None
  in
  Sys.readdir dir |> Array.to_list |> List.filter_map make_test