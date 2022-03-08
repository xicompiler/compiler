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

type nonrec result = (Ast.Decorated.t, error) result

let parse_prog =
  Fn.compose (Result.map_error ~f:coerce) Parse.parse_prog

let parse_source lb =
  Fn.compose (Result.map_error ~f:coerce) Parse.parse_source lb
  >>| Ast.source

let parse_intf lb =
  Fn.compose (Result.map_error ~f:coerce) Parse.parse_intf lb
  >>| Ast.intf

let map ~f = File.Xi.map ~source:(f parse_source) ~intf:(f parse_intf)
let semantic_error e = `SemanticError e

let type_check ?cache ~deps ast =
  try
    ast
    |> type_check ~find_intf:(find_intf ?cache ~deps)
    |> Result.map_error ~f:semantic_error
  with Parse.Exn e -> Error (coerce e)

let type_check_file ?cache ~deps file =
  Parse.File.map_ast ~f:(type_check ?cache ~deps) file >>| function
  | Ok ok -> ok
  | Error err -> Error (coerce err)

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

  let print_result ~out = function
    | Ok _ -> Printf.fprintf out "%s\n" valid_xi_program
    | Error e -> e |> Error.to_string |> Printf.fprintf out "%s\n"

  let print_to_file ~out r =
    Out_channel.with_file ~f:(fun oc -> print_result ~out:oc r) out

  let file_to_file ?cache ~src ~out ~deps () =
    let f = Fn.compose (print_to_file ~out) (type_check ?cache ~deps) in
    Result.ignore_m (Parse.File.map_ast ~f src)
end