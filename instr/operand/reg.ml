open Core
open Util.Fn

module Bit64 = struct
  type t =
    [ `rax
    | `rbx
    | `rcx
    | `rdx
    | `rsi
    | `rdi
    | `rsp
    | `rbp
    | `rip
    | `r8
    | `r9
    | `r10
    | `r11
    | `r12
    | `r13
    | `r14
    | `r15
    ]
  [@@deriving variants]
end

module Bit8 = struct
  type t =
    [ `ah
    | `al
    | `bh
    | `bl
    | `ch
    | `cl
    | `dh
    | `dl
    ]
  [@@deriving variants]
end

type t =
  [ Bit64.t
  | Bit8.t
  ]

type concrete = t

let to_string : [< t ] -> string = function
  | #Bit64.t as r -> Bit64.Variants.to_name r
  | #Bit8.t as r -> Bit8.Variants.to_name r

module Abstract = struct
  type t =
    [ concrete
    | Ir.Temp.Virtual.t
    ]

  let to_string : [< t ] -> string = function
    | #concrete as reg -> to_string reg
    | #Ir.Temp.Virtual.t as t -> Ir.Temp.Virtual.to_string t
end
