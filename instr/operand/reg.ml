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
  [@@deriving variants, equal, sexp, compare, hash]
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
    | `r8b
    ]
  [@@deriving variants, equal, sexp, compare, hash]

  let to_64_bit = function
    | `ah | `al -> `rax
    | `bh | `bl -> `rbx
    | `ch | `cl -> `rcx
    | `dh | `dl -> `rdx
    | `r8b -> `r8
end

type concrete =
  [ Bit64.t
  | Bit8.t
  ]
[@@deriving equal, sexp, compare, hash]

type t = concrete [@@deriving equal]

let to_64_bit = function
  | #Bit64.t as r -> r
  | #Bit8.t as r -> Bit8.to_64_bit r

let to_string : [< t ] -> string = function
  | #Bit64.t as r -> Bit64.Variants.to_name r
  | #Bit8.t as r -> Bit8.Variants.to_name r

module Abstract = struct
  module Args = struct
    type t =
      [ concrete
      | Ir.Temp.Virtual.t
      ]
    [@@deriving sexp, compare, hash]
  end

  include Args

  let to_string : [< t ] -> string = function
    | #concrete as reg -> to_string reg
    | #Ir.Temp.Virtual.t as t -> Ir.Temp.Virtual.to_string t

  include Comparable.Make (Args)
  include Hashable.Make (Args)
end
