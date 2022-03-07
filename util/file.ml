open Core

let xi s = s ^ ".xi"
let ixi s = s ^ ".ixi"
let ixi_of_dir ~dir file = Filename.concat dir (ixi file)

let accessible path =
  match Sys.file_exists path with
  | `Yes -> true
  | `Unknown | `No -> false

let stdlib = "./xi-libraries"
