let printable_ascii_min = Char.chr 32

let printable_ascii_max = Char.chr 127

let fold f =
  let folder acc _ = function
    | `Uchar u -> f acc u
    | `Malformed _ -> invalid_arg "Input string malformed"
  in
  Uutf.String.fold_utf_8 folder

let iter f = fold (fun () -> f) ()

let uchars_of_string s =
  let uchar_folder us u = List.cons u us in
  let seq_builder us u = Seq.cons u us in
  s |> fold uchar_folder [] |> List.fold_left seq_builder Seq.empty
