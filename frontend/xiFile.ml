open Core

(** [file_not_found s] is "s: No such file" *)
let file_not_found = Printf.sprintf "%s: No such file"

(** [not_xi_file s] is "s: Not a Xi file" *)
let not_xi_file = Printf.sprintf "%s: Not a Xi file"

type nonrec 'a file_result = ('a, string) result
(** A [result] is either [Ok ()] or [Error msg] where [msg] is a string
    detailing the error. *)

type 'a apply = Lexing.lexbuf -> 'a file_result
(** [apply] is the type of a function that can be applied to a lexer
    buffer and yields a [result]. *)

(** [join_error r] is [e] if [r] is [Error e] and [r] otherwise*)
let join_error = function
  | Error e -> e
  | Ok _ as ok -> ok

(** [try_apply file apply] is [apply lexbuf] if [lexbuf] is a lexer
    buffer created from [file], or [Error] if an error ocurrs while
    opening the file. *)
let try_apply file (apply : 'a apply) =
  let map () =
    let f ic = ic |> Lexing.from_channel |> apply in
    In_channel.with_file ~f file
  in
  let map_err _ = Error (file_not_found file) in
  map |> Result.try_with
  |> Result.map_error ~f:map_err
  |> join_error |> Result.join

let bind ~source ~interface file =
  match Caml.Filename.extension file with
  | ".xi" -> try_apply file source
  | ".ixi" -> try_apply file interface
  | _ -> Error (not_xi_file file)

(** [fold_error e acc] is [Error (e :: es)] if [acc] is [Error es] and
    [Error \[e\]] if [acc] is [Ok ()] *)
let fold_error e = function
  | Error es -> Error (e :: es)
  | Ok () -> Error [ e ]

let iter_all ~source ~interface files =
  let f acc file =
    match bind ~source ~interface file with
    | Ok () -> acc
    | Error e -> fold_error e acc
  in
  let init = Ok () in
  files |> List.fold_left ~f ~init |> Result.map_error ~f:List.rev
