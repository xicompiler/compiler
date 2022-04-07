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

  type abstract =
    [ t
    | Ir.temp
    ]
end

type t =
  [ Bit64.t
  | Bit8.t
  ]

type abstract =
  [ t
  | Ir.temp
  ]

let to_string : [< t ] -> string = function
  | #Bit64.t as r -> Bit64.Variants.to_name r
  | #Bit8.t as r -> Bit8.Variants.to_name r
