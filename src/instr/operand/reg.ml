open Core
open Util.Fn

module Bit64 = struct
  module Temp = struct
    module T = struct
      type t =
        [ `rax
        | `rbx
        | `rcx
        | `rdx
        | `rsi
        | `rdi
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

    include T
    module Table = Hashtbl.Make (T)

    let reg_array : t array =
      [|
        `rax;
        `rcx;
        `rdx;
        `rsi;
        `rdi;
        `r8;
        `r9;
        `r10;
        `r11;
        `rbx;
        `r12;
        `r13;
        `r14;
        `r15;
      |]

    let of_int_exn = Array.get reg_array

    let to_int_exn =
      let size = Array.length reg_array in
      let tbl = Table.create ~size () in
      Array.iteri reg_array ~f:(fun i reg ->
          Hashtbl.add_exn tbl ~key:reg ~data:i);
      Hashtbl.find_exn tbl

    let to_string = Variants.to_name
  end

  type t =
    [ Temp.t
    | `rsp
    | `rbp
    | `rip
    ]
  [@@deriving equal, sexp, compare, hash]

  let to_string : [< t ] -> string = function
    | #Temp.t as t -> Temp.to_string t
    | `rsp -> "rsp"
    | `rbp -> "rbp"
    | `rip -> "rip"

  let to_8_bit = function
    | `rax -> `al
    | `rbx -> `bl
    | `rcx -> `cl
    | `rdx -> `dl
    | `r8 -> `r8b
    | #t -> failwith "no 8bit representation"
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

let to_8_bit = function
  | #Bit64.t as r -> Bit64.to_8_bit r
  | #Bit8.t as r -> r

let to_string : [< t ] -> string = function
  | #Bit64.t as r -> Bit64.to_string r
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

  let to_64_bit = function
    | #concrete as reg -> to_64_bit reg
    | abstract -> abstract

  include Comparable.Make (Args)
  include Hashable.Make (Args)
end
