open Core

let no_such_file = Printf.sprintf "%s: No such file"

type error = string
(** [error] is an alias for [Error.t] *)

type nonrec 'a result = ('a, error) result
(** A [result] is either [Ok] of ['a] or [Error] *)

let map ~f file =
  let f = Fn.compose f Lexing.from_channel in
  (fun () -> In_channel.with_file ~f file)
  |> Option.try_with
  |> Result.of_option ~error:file
