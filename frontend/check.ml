open Core
open Result.Let_syntax
open Ast

module Error = struct
  type type_error = Decorated.Error.error

  type t =
    [ Parse.error
    | `SemanticError of type_error
    ]

  (** [desc e] is the error description corresponding to [e] *)
  let desc = Type.Error.to_string

  let string_of_cause cause = cause |> desc |> Printf.sprintf "error:%s"

  (** [fmt] is the format for a lexical error message *)
  let fmt = format_of_string "Semantic error beginning at %s:%s"

  let to_string filename = function
    | #Parse.error as e -> Parse.Error.to_string filename e
    | `SemanticError e ->
        let pos = Position.Error.position e in
        e |> Position.Error.cause |> desc |> Position.Error.format pos
end

type error = Error.t

let type_check ~lib_dir lexbuf =
  let find_intf file =
    let path = Filename.concat lib_dir (Util.File.ixi file) in
    match Parse.parse_intf_file path with
    | Ok (Ok sigs) -> Some sigs
    | Ok (Error e) -> raise (Parse.Exn e)
    | Error _ -> None
    (* throw errors here *)
  in
  let%bind prog =
    Parse.parse_prog lexbuf
    |> Result.map_error ~f:(fun e -> (e :> error))
  in
  type_check ~find_intf prog
  |> Result.map_error ~f:(fun e -> `SemanticError e)

module Diagnostic = struct
  module Error = struct
    let to_string = function
      | #Parse.error as e -> Parse.Diagnostic.Error.to_string e
      | `SemanticError e ->
          let pos = Position.Error.position e in
          e |> Position.Error.cause |> Error.string_of_cause
          |> Position.Error.format pos
  end

  (** [valid_xi_program] is the diagnostic string printed if a program
      is semantically valid *)
  let valid_xi_program = "Valid Xi Program"

  let print_res out = function
    | Ok _ -> Printf.fprintf out "%s\n" valid_xi_program
    | Error e -> e |> Error.to_string |> Printf.fprintf out "%s\n"

  let to_channel lexbuf ~lib_dir out =
    type_check ~lib_dir lexbuf |> print_res out

  let to_file ~lib_dir lexbuf out =
    Out_channel.with_file ~f:(to_channel lexbuf ~lib_dir) out

  let file_to_file ~src ~lib_dir ~out =
    File.Xi.map_same ~f:(fun buf -> to_file ~lib_dir buf out) src
end