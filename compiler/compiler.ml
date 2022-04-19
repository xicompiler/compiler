open Core
open Core_unix
open Result.Let_syntax
open Frontend
open Util.File
module Args = Args

type nonrec result = (unit, string list) result

open Args

(** [lex_out ~dir src] writes the lexing diagnostic file of [src] *)
let lex_out ~dir ~src =
  let out = Util.File.diagnostic ~dir ~src ".lexed" in
  Lex.Diagnostic.file_to_file ~src ~out

(** [parse_out ~dir src] writes the parsing diagnostic file of [src] *)
let parse_out ~dir ~src =
  let out = Util.File.diagnostic ~dir ~src ".parsed" in
  Parse.Diagnostic.file_to_file ~src ~out

(** [check_out ~dir src] writes the typechecking diagnostic file of
    [src] *)
let check_out ?cache ~dir ~src ~deps () =
  let out = Util.File.diagnostic ~dir ~src ".typed" in
  Check.Diagnostic.file_to_file ?cache ~src ~out ~deps ()

(** [ir_run ir] interprets and executes the ir file at path [ir] *)
let ir_run ir =
  let command = Printf.sprintf "./irrun %s" ir in
  ignore (Sys.command command)

(** [ir_out ?cache ~args ~dir ~src ~deps ()] outputs IR of file with
    path [src] writing the results to a file in directory [dir] *)
let ir_out ?cache ~args ~dir ~src ~deps () =
  let out = Util.File.diagnostic ~dir ~src ".ir" in
  let optimize = not args.disable_optimize in
  if Util.File.is_xi src then begin
    let open Ir.Output in
    ignore (file_to_file ?cache ~src ~out ~deps ~optimize ());
    if args.irrun then ir_run out
  end

(** [abstract_asm_out ?cache ~args ~dir ~src ~deps ()] outputs ASM of
    file with path [src] writing the results to a file in directory
    [dir] *)
let abstract_asm_out ?cache ~args ~dir ~src ~deps () =
  let out = Util.File.diagnostic ~dir ~src ".asm" in
  let optimize = not args.disable_optimize in
  if Util.File.is_xi src then
    let open Instr.Output.Abstract in
    ignore (file_to_file ?cache ~src ~out ~deps ~optimize ())

(** [asm_run asm] interprets and executes the asm file at path [asm] *)
let asm_run asm =
  let command = Printf.sprintf "./asmrun %s" asm in
  ignore (Sys.command command)

(** [asm_out ?cache ~args ~dir ~src ~deps ()] outputs ASM of file with
    path [src] writing the results to a file in directory [dir] *)
let asm_out ?cache ~args ~dir ~src ~deps () =
  let out = Util.File.diagnostic ~dir ~src ".s" in
  let optimize = not args.disable_optimize in
  if Util.File.is_xi src then (
    let open Instr.Output in
    ignore (file_to_file ?cache ~src ~out ~deps ~optimize ());
    if args.asmrun then asm_run out)

(** [deps_of_args args] are the semantic dependecies corresponding to
    [args] *)
let deps_of_args { lib_dir; std_dir; _ } : Check.dependencies =
  { lib_dir; std_dir }

(** [target_of_args args] is the target corresponding to [args], or
    ["linux"] if the target is not supported *)
let target_of_args { target; _ } =
  if not (String.equal target "linux") then
    Printf.printf "%s is not a supported target\n" target;
  "linux"

(** [compile_file ?cache ~args src] compiles file at path [src] and is
    [Ok ()] on success or [Error e] on failure, where [e] is an error
    message. *)
let compile_file ?cache ~args src =
  let src_path = Filename.concat args.src_dir src in
  let deps = deps_of_args args in
  let dir = args.asm_out_dir in
  let _ = target_of_args args in
  asm_out ?cache ~args ~dir ~src ~deps ();
  let%map r = Check.type_check_file ?cache ~deps src_path in
  r
  |> Result.map_error ~f:(Check.Error.to_string src_path)
  |> Result.ignore_m

(** [compile_file_options ?cache ~args src] compiles file at path [src]
    with command line arguments [args] *)
let compile_file_options ?cache ~args src =
  let dir = args.diag_out_dir in
  let deps = deps_of_args args in
  if args.lex then ignore (lex_out ~dir ~src);
  if args.parse then ignore (parse_out ~dir ~src);
  if args.typecheck then ignore (check_out ?cache ~dir ~src ~deps ());
  if args.irgen || args.irrun then
    ir_out ?cache ~args ~dir ~src ~deps ();
  if args.abstract_asm then
    abstract_asm_out ?cache ~args ~dir ~src ~deps ();
  compile_file ?cache ~args src

(** [stringify res] is an error message string if [res] is an error *)
let stringify = function
  | Ok ok -> ok
  | Error e -> Error (File.Xi.Error.to_string e)

let compile args =
  let cache = String.Table.create () in
  let invoke = compile_file_options ~cache ~args in
  args.files
  |> List.map ~f:(Fn.compose stringify invoke)
  |> Result.combine_errors_unit
