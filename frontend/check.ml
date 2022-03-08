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
        e |> Position.Error.cause |> desc
        |> Position.Error.format pos
        |> Printf.sprintf fmt filename
end

type error = Error.t
type cache = Ast.Toplevel.intf option String.Table.t

type dependencies = {
  lib_dir : string;
  std_dir : string;
}

let get_path { lib_dir; std_dir } intf =
  let lib_path = Util.File.ixi_of_dir ~dir:lib_dir intf in
  if Util.File.accessible lib_path then lib_path
  else Util.File.ixi_of_dir ~dir:std_dir intf

let parse_intf ~deps intf =
  match Parse.File.parse_intf (get_path deps intf) with
  | Ok (Ok sigs) -> Some sigs
  | Ok (Error e) -> raise (Parse.Exn e)
  | Error _ -> None

let find_intf ?cache ~deps intf =
  let default = parse_intf ~deps in
  match cache with
  | Some cache -> Hashtbl.findi_or_add cache intf ~default
  | None -> default intf

let coerce e = (e :> error)

let parse_prog =
  Fn.compose (Result.map_error ~f:coerce) Parse.parse_prog

let parse_source lb =
  Fn.compose (Result.map_error ~f:coerce) Parse.parse_source lb
  >>| Ast.source

let parse_intf lb =
  Fn.compose (Result.map_error ~f:coerce) Parse.parse_intf lb
  >>| Ast.intf

let semantic_error e = `SemanticError e

let type_check ?cache ~deps lexbuf =
  let%bind prog = parse_prog lexbuf in
  try
    prog
    |> type_check ~find_intf:(find_intf ?cache ~deps)
    |> Result.map_error ~f:semantic_error
  with Parse.Exn e -> Error (coerce e)

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

  let to_channel ?cache ~deps lexbuf out =
    lexbuf |> type_check ?cache ~deps |> print_res out

  let to_file ?cache ~deps lexbuf out =
    Out_channel.with_file ~f:(to_channel ?cache ~deps lexbuf) out

  let file_to_file ?cache ~src ~out ~deps () =
    File.Xi.map_same_fn ~f:(fun buf -> to_file ?cache ~deps buf out) src
end