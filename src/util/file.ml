open Fn
open Core
open Core_unix

let xi s = s ^ ".xi"
let ixi s = s ^ ".ixi"
let ixi_of_dir ~dir file = Filename.concat dir (ixi file)
let is_xi = Caml.Filename.extension >> String.equal ".xi"

let accessible path =
  match Sys_unix.file_exists path with
  | `Yes -> true
  | `Unknown | `No -> false

let stdlib = "./xi-libraries"

(** [replace_ext ~ext ~file] is the string [chopped.ext] where [chopped]
    is [file] with its extension removed. *)
let replace_ext ~ext ~file = Filename.chop_extension file ^ ext

(** [make_out_path ~dir ~file ~ext] is the path of parse diagnostics
    file ending in [ext] in directory [dir] for file [file] *)
let make_out_path ~dir ~file ext =
  let file = replace_ext ~file ~ext in
  Filename.concat dir file

(** [create_out_path ~dir ~file ext] creates the directory of
    [make_out_path ~dir ~file ext] if absent and returns the result. *)
let create_out_path ~dir ~file ext =
  let path = make_out_path ~dir ~file ext in
  mkdir_p (Filename.dirname path);
  path

let diagnostic ~dir ~src = create_out_path ~dir ~file:src
let base = Fn.compose Filename.chop_extension Filename.basename

let println s ~out =
  Out_channel.with_file ~f:(fun oc -> Printf.fprintf oc "%s\n" s) out
