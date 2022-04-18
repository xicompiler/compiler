open Core
open Util.Fn

(** [t] is the type of a generic instruction in x86 *)
type 'a t =
  | Label of Ir.label
  | Enter of Imm.t * Imm.t
  | Jmp of 'a
  | Jcc of ConditionCode.t * Ir.label
  | Setcc of ConditionCode.t * 'a
  | Cmp of 'a * 'a
  | Test of 'a * 'a
  | Push of 'a
  | Pop of 'a
  | IMul of [ `M of 'a | `RM of 'a * 'a | `RMI of 'a * 'a * Imm.t ]
  | Inc of 'a
  | Dec of 'a
  | Call of 'a
  | IDiv of 'a
  | Shl of 'a * Imm.t
  | Shr of 'a * Imm.t
  | Sar of 'a * Imm.t
  | Add of 'a * 'a
  | Sub of 'a * 'a
  | Xor of 'a * 'a
  | And of 'a * 'a
  | Or of 'a * 'a
  | Lea of 'a * 'a
  | Mov of 'a * 'a
  | Movzx of 'a * 'a
  | Leave
  | Ret
[@@deriving variants]

(** [fmt1] represents the format of a 1-operand instruction in x86 *)
let fmt1 = format_of_string "%s %s"

(** [fmt2] represents the format of a 2-operand instruction in x86 *)
let fmt2 = format_of_string "%s %s, %s"

(** [fmt3] represents the format of a 3-operand instruction in x86 *)
let fmt3 = format_of_string "%s %s, %s, %s"

(** [format1 ~f ~name e] is the string representation of the instruction
    [name e] *)
let format1 ~f ~name = f >> Printf.sprintf fmt1 name

(** [format_operand_string ~f ~name e s] is the string representation of
    the intruction [name e, s] *)
let format_operand_string ~f ~name e = Printf.sprintf fmt2 name (f e)

(** [format2 ~f ~name e1 e2] is the string representation of the
    instruction [name e1, e2] *)
let format2 ~f ~name e1 = f >> format_operand_string ~f ~name e1

(** [format_mi ~f ~name e i] is the string representation of the
    instruction [name e, i], where [i] is an immediate value *)
let format_mi ~f ~name e =
  Int64.to_string >> format_operand_string ~f ~name e

(** [format_cc ~prefix cc s] is the string representation of the
    instruction [prefixcc s]*)
let format_cc ~prefix =
  ConditionCode.to_string >> Printf.sprintf "%s%s %s" prefix

(** [format_rmi ~f ~name e1 e2 i] is the string representation of the
    instruction [name e1, e2, e3] *)
let format_rmi ~f ~name e1 e2 =
  let s1 = f e1 in
  let s2 = f e2 in
  Int64.to_string >> Printf.sprintf fmt3 name s1 s2

(** [string_of_imul ~f mul] is the string representation of the
    instruction [imul mul] *)
let string_of_imul ~f =
  let name = "imul" in
  function
  | `M d -> format1 ~f ~name d
  | `RM (r, d) -> format2 ~f ~name r d
  | `RMI (r, d, i) -> format_rmi ~f ~name r d i

(** [string_of_setcc ~f cc r] is the string representation of the
    instruction [setcc r] *)
let string_of_setcc ~f cc = f >> format_cc ~prefix:"set" cc

(** [string_of_jcc cc l] is the string representation of the instruction
    [jcc l] *)
let string_of_jcc = format_cc ~prefix:"j"

(** [string_of_enter i1 i2] is the string representation of the
    instruction [enter i1 i2] *)
let string_of_enter = Printf.sprintf "enter %Ld, %Ld"

(** [string_of_lea ~f r m] is the string representation of the
    instruction [lea r, \[m\]]*)
let string_of_lea ~f r =
  Mem.to_string >> format_operand_string ~f ~name:"lea" r

let to_string ~f : 'a t -> string = function
  | Label l -> l ^ ":"
  | Enter (i1, i2) -> string_of_enter i1 i2
  | Jmp n -> format1 ~f ~name:"jmp" n
  | Jcc (cc, l) -> string_of_jcc cc l
  | Setcc (cc, r) -> string_of_setcc ~f cc r
  | Cmp (d, o) -> format2 ~f ~name:"cmp" d o
  | Test (d, o) -> format2 ~f ~name:"test" d o
  | Push o -> format1 ~f ~name:"push" o
  | Pop d -> format1 ~f ~name:"pop" d
  | IMul m -> string_of_imul ~f m
  | Inc d -> format1 ~f ~name:"inc " d
  | Dec d -> format1 ~f ~name:"dec" d
  | Call n -> format1 ~f ~name:"call" n
  | IDiv d -> format1 ~f ~name:"idiv" d
  | Shl (d, i) -> format_mi ~f ~name:"shl" d i
  | Shr (d, i) -> format_mi ~f ~name:"shr" d i
  | Sar (d, i) -> format_mi ~f ~name:"sar" d i
  | Add (d, o) -> format2 ~f ~name:"add" d o
  | Sub (d, o) -> format2 ~f ~name:"sub" d o
  | Xor (d, o) -> format2 ~f ~name:"xor" d o
  | And (d, o) -> format2 ~f ~name:"and" d o
  | Or (d, o) -> format2 ~f ~name:"or" d o
  | Lea (r, m) -> format2 ~f ~name:"lea" r m
  | Mov (d, o) -> format2 ~f ~name:"mov" d o
  | Movzx (d, o) -> format2 ~f ~name:"movzx" d o
  | Leave -> "leave"
  | Ret -> "ret"

let data_fmt = format_of_string "%s: .quad %s"

(** [string_of_data l is] is the representation of static data with name
    [l] and contents [is] *)
let string_of_data l =
  List.map ~f:Int64.to_string
  >> String.concat ~sep:", "
  >> Printf.sprintf data_fmt l

let jnz l = Jcc (ConditionCode.Nz, l)
let zero e = Xor (e, e)

type 'a instr = 'a t

module Asm = struct
  let label_of_string = Printf.sprintf "%s:"

  module Data = struct
    type t = {
      label : Ir.label;
      value : int64 list;
    }
    [@@deriving fields]

    let create = Fields.create

    (** [to_string { label; value = \[i1; ...; in\] }] is "<label>:
        <i1>, ..., <in>" *)
    let to_string { label; value } =
      value
      |> List.map ~f:Int64.to_string
      |> String.concat ~sep:", "
      |> Printf.sprintf "%s: .quad %s" label
  end

  module Fn = struct
    type 'a body = 'a instr list

    type 'a t = {
      name : string;
      body : 'a body;
    }
    [@@deriving fields]

    let create = Fields.create
    let map_body ~f fn = { fn with body = f fn.body }

    (** [to_lines ~f { name; body = \[s1; ...; sn\] }] is
        [\[":<name>"; l1; ...; ln\]] where each [li] is the string
        representation of [li] *)
    let to_lines ~f { name; body } =
      let f = to_string ~f >> Printf.sprintf "\t%s" in
      label_of_string name :: List.map ~f body
  end

  module Directive = struct
    (** [Prefix] represents the prefix in AT&T syntax *)
    module Prefix = struct
      type t =
        [ `prefix
        | `noprefix
        ]

      (** [to_string prefix] is the string representation of [prefix] *)
      let to_string = function
        | `prefix -> "prefix"
        | `noprefix -> "noprefix"
    end

    type 'a t =
      | Text of 'a Fn.t list
      | Data of Data.t list
      | Globl of Ir.label list
      | IntelSyntax of [ `prefix | `noprefix ]

    (** [string_of_globl \[sym1; ...; symn\]] is ".globl sym1, ...,
        symn"*)
    let string_of_globl syms = ".globl " ^ String.concat ~sep:", " syms

    (** [lines_of_text ~f fns] is [\[".text"; l1; ...; ln\]] where
        [\[l1; ...; ln\]] a list of assembly lines, each of which
        corresponds to the instruction of a function in [fns], where
        their operands are stringified using [f] *)
    let lines_of_text ~f fns =
      ".text" :: List.concat_map fns ~f:(Fn.to_lines ~f)

    (** [lines_of_data ~f \[j1; ...; jn\]] is [\[".data"; l1; ...; ln\]]
        where each [li] is the stringification of [ji] *)
    let lines_of_data ~f data =
      ".data" :: List.map ~f:Data.to_string data

    (** [string_of_intel_sytax prefix] is [".intel_syntax <prefix>"]
        where [<prefix>] is the string representation of [prefix] *)
    let string_of_intel_syntax =
      Prefix.to_string >> Printf.sprintf ".intel_syntax %s"

    let to_lines ~f = function
      | Text fns -> lines_of_text ~f fns
      | Data data -> lines_of_data ~f data
      | Globl syms -> [ string_of_globl syms ]
      | IntelSyntax prefix -> [ string_of_intel_syntax prefix ]
  end

  let to_lines ~f = List.concat_map ~f:(Directive.to_lines ~f)
  let to_string ~f = to_lines ~f >> String.concat ~sep:"\n"

  type 'a t = 'a Directive.t list
end