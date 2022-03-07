open Core
open Util.Result

(** [no_such_file s] is "s: No such file" *)
let no_such_file = Printf.sprintf "%s: No such file"

(** [not_xi_file s] is "s: Not a Xi file" *)
let not_xi_file = Printf.sprintf "%s: Not a Xi file"

type nonrec 'a result = ('a, string) result
type 'a bind = Lexing.lexbuf -> 'a result

(** [try_apply file apply] is [apply lexbuf] if [lexbuf] is a lexer
    buffer created from [file], or [Error] if an error ocurrs while
    opening the file. *)
let try_apply file apply =
  let map () =
    let f ic = ic |> Lexing.from_channel |> apply in
    In_channel.with_file ~f file
  in
  let error () = Error (no_such_file file) in
  map |> Option.try_with |> Lazy.of_option ~error |> join_error
  |> Result.join

let bind ~source ~intf file =
  match Caml.Filename.extension file with
  | ".xi" -> try_apply file source
  | ".ixi" -> try_apply file intf
  | _ -> Error (not_xi_file file)

let bind_same ~f = bind ~source:f ~intf:f

type 'a map = Lexing.lexbuf -> 'a

let map ~source ~intf =
  let lift f x = Ok (f x) in
  bind ~source:(lift source) ~intf:(lift intf)

let map_same ~f = map ~source:f ~intf:f
