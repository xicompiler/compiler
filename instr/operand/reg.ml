open Core
open Util.Fn

type t =
  [ `Rax
  | `Rbx
  | `Rcx
  | `Rdx
  | `Rsi
  | `Rdi
  | `Rsp
  | `Rbp
  | `R9
  | `R10
  | `R11
  | `R12
  | `R13
  | `R14
  | `R15
  ]
[@@deriving variants]

let to_string : [< t ] -> string = Variants.to_name >> String.lowercase
