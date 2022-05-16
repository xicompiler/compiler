open Core
open Result.Let_syntax
open Ast
open Util.Fn

module Error = struct
  type t =
    [ Parse.error
    | `SemanticError of Type.Error.Positioned.t
    ]

  (** [desc e] is the error description corresponding to [e] *)
  let desc = Type.Error.to_string

  let string_of_cause cause = cause |> desc |> Printf.sprintf "error:%s"

  (** [fmt] is the format for a lexical error message *)
  let fmt = format_of_string "Semantic error beginning at %s:%s"

  let to_string filename = function
    | #Parse.error as e -> Parse.Error.to_string filename e
    | `SemanticError e ->
        Position.Error.format_last ~f:desc ~msg:filename ~fmt e
end

type error = Error.t
type cache = (Ast.Undecorated.intf option, exn) result String.Table.t

type dependencies = {
  lib_dir : string;
  std_dir : string;
}

let get_rho_path ~file lib_dir std_dir intf =
  let lib_path = Util.File.ri_of_dir ~dir:lib_dir intf in
  if Util.File.accessible lib_path then lib_path
  else
    let lib_path2 = Util.File.ri_of_dir ~dir:std_dir intf in
    if Util.File.accessible lib_path2 then lib_path2
    else
      let lib_path3 = Util.File.ixi_of_dir ~dir:lib_dir intf in
      if Util.File.accessible lib_path3 then lib_path3
      else Util.File.ixi_of_dir ~dir:std_dir intf

let get_xi_path ~file lib_dir std_dir intf =
  let lib_path = Util.File.ixi_of_dir ~dir:lib_dir intf in
  if Util.File.accessible lib_path then lib_path
  else Util.File.ixi_of_dir ~dir:std_dir intf

let get_path ~file { lib_dir; std_dir } intf =
  if Util.File.is_xi file then get_xi_path ~file lib_dir std_dir intf
  else get_rho_path ~file lib_dir std_dir intf

(** [parse_intf ~deps intf] is [Ok (Some ast)] if [ast] is the interface
    ast parsed from file [intf], [Ok None] if file [intf] does not exist
    and [Error exn] on syntax error. *)
let parse_intf ~file ~deps intf =
  match Parse.File.parse_intf (get_path deps ~file intf) with
  | Ok (Ok sigs) -> Ok (Some sigs)
  | Ok (Error e) -> Error (Parse.Exn e)
  | Error _ -> Ok None

(** [find_intf ~cache ~deps intf] is [parse_intf ~deps intf], caching
    the result in [cache] if provided *)
let find_intf ?cache ~file ~deps intf =
  let default = parse_intf ~file ~deps in
  match cache with
  | Some cache -> Hashtbl.findi_or_add cache intf ~default
  | None -> default intf

(** Same as [find_intf], but raises [exn] on [Error exn] *)
let find_intf_exn ?cache ~file ~deps =
  Fn.compose Result.ok_exn (find_intf ?cache ~file ~deps)

let coerce e = (e :> error)

type nonrec result = (Ast.Decorated.t, error) result

let parse_prog ~is_rho =
  Fn.compose (Result.map_error ~f:coerce) (Parse.parse_prog ~is_rho)

let parse_source ~is_rho lb =
  Fn.compose
    (Result.map_error ~f:coerce)
    (Parse.parse_source ~is_rho)
    lb
  >>| Ast.source

let parse_intf ~is_rho lb =
  Fn.compose (Result.map_error ~f:coerce) (Parse.parse_intf ~is_rho) lb
  >>| Ast.intf

let map ~is_rho ~f =
  File.Xi.map
    ~source:(f parse_source ~is_rho)
    ~intf:(f parse_intf ~is_rho)

let semantic_error e = `SemanticError e

let type_check ~file ?cache ~deps ast =
  try
    ast
    |> Undecorated.type_check ~file
         ~find_intf:(find_intf_exn ?cache ~file ~deps)
    |> Result.map_error ~f:semantic_error
  with Parse.Exn e -> Error (coerce e)

let type_check_file ?cache ~deps file =
  Parse.File.map_ast ~f:(type_check ~file ?cache ~deps) file
  >>| function
  | Ok ok -> ok
  | Error err -> Error (coerce err)

module Diagnostic = struct
  module Error = struct
    let to_string = function
      | #Parse.error as e -> Parse.Diagnostic.Error.to_string e
      | `SemanticError e ->
          Position.Error.format ~f:Error.string_of_cause e
  end

  (** [valid_xi_program] is the diagnostic string printed if a program
      is semantically valid *)
  let valid_xi_program = "Valid Xi Program"

  (** [iter_result ~ok ~err r] is [err e] if [r] is [Error e] and is
      [ok ast] if [r] is [Ok ast] *)
  let iter_result ~ok ~err = function
    | Ok ast -> ok ast
    | Error e -> err e

  let iter_file
      ?cache
      ~src
      ~deps
      ?(ok = fun _ -> ())
      ?(err = fun _ -> ())
      () =
    let r = type_check_file ?cache ~deps src in
    Result.iter ~f:(iter_result ~ok ~err) r;
    r

  let file_to_file ?cache ~src ~out ~deps =
    let ok _ = Util.File.println valid_xi_program ~out in
    let err = Error.to_string >> Util.File.println ~out in
    iter_file ?cache ~src ~deps ~ok ~err
end
