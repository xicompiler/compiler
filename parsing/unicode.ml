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
  let seq_builder us u () = Seq.Cons (u, us) in
  s |> fold uchar_folder [] |> List.fold_left seq_builder Seq.empty

(** [escape_unicode u] is [u] escaped in the form [\x{n}], where [n] is
    the hexidecimal code of [u] *)
let escape_unicode u = u |> Uchar.to_int |> Printf.sprintf "\\x{%x}"

let string_of_uchar u =
  match Uchar.to_char u with
  | ('\n' | '\t' | '\r' | '\b') as c -> Char.escaped c
  | c when c >= printable_ascii_min && c <= printable_ascii_max ->
      Char.escaped c
  | _
  | (exception _) ->
      escape_unicode u

let escape_string s =
  let buf = s |> String.length |> Buffer.create in
  let itr u = u |> string_of_uchar |> Buffer.add_string buf in
  iter itr s;
  Buffer.contents buf
