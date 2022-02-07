let printable_ascii_min = Char.chr 32

let printable_ascii_max = Char.chr 127

let fold f =
  let folder acc _ = function
    | `Uchar u -> f acc u
    | `Malformed _ -> invalid_arg "Input string malformed"
  in
  Uutf.String.fold_utf_8 folder

let iter f = fold (fun () -> f) ()
