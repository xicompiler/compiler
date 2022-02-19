open Core

(** [make_path ~dir ~file] is the string [dir/file] *)
let make_path ~dir ~file = Printf.sprintf "%s/%s" dir file

(** [replace_ext ~ext ~file] is the string [chopped.ext] where [chopped]
    is [file] with its extension removed. *)
let replace_ext ~ext ~file = Filename.chop_extension file ^ ext

(** [try_iter thunk] evaluates [thunk ()] and is [()] regardless of
    whether the evaluation raises an exception. *)
let try_iter thunk = thunk |> Result.try_with |> ignore

(** [diagnostic ~src_dir ~out_dir ~ext ~f] is [f ~src ~out] where [f] is
    a supplied diagnostic function, [src] is the path to the diagnostic
    source file, and [out] is the path to the diagnostic out file. *)
let diagnostic ~src_dir ~out_dir ~ext ~f src_file =
  let out_file = replace_ext ~file:src_file ~ext in
  let out_path = make_path ~dir:out_dir ~file:out_file in
  let src_path = make_path ~dir:src_dir ~file:src_file in
  try_iter (fun () -> Core.Unix.mkdir_p out_dir);
  f ~src:src_path ~out:out_path

let compile
    ?(lex = false)
    ?(parse = false)
    ?(src_dir = ".")
    ?(out_dir = ".")
    files =
  failwith "unimplemented"
