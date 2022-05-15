open Core
open Util.Fn

module Bit64 = struct
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
      | `rsp
      | `rbp
      | `rip
      ]
    [@@deriving variants, equal, sexp, compare, hash]
  end

  include T

  let caller_save : [> t ] Sequence.t =
    Sequence.of_list
      [ `rax; `rcx; `rdx; `rsi; `rdi; `r8; `r9; `r10; `r11 ]

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
      `rsp;
      `rbp;
      `rip;
    |]

  let of_int_exn = Array.get reg_array

  let to_int_exn =
    let size = Array.length reg_array in
    let tbl = Table.create ~size () in
    Array.iteri reg_array ~f:(fun i reg ->
        Hashtbl.add_exn tbl ~key:reg ~data:i);
    Hashtbl.find_exn tbl

  let to_string = Variants.to_name

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

type t =
  [ Bit64.t
  | Bit8.t
  ]
[@@deriving equal, sexp, compare, hash]

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
      [ Bit64.t
      | Ir.Temp.Virtual.t
      ]
    [@@deriving equal, sexp, compare, hash]
  end

  include Args

  let to_string : [< t ] -> string = function
    | #Bit64.t as r -> Bit64.to_string r
    | #Ir.Temp.Virtual.t as t -> Ir.Temp.Virtual.to_string t

  let to_int : t -> int option = function
    | #Bit64.t as r -> Some (Bit64.to_int_exn r)
    | #Ir.Temp.Virtual.t -> None

  let iter_temp r ~f =
    match r with #Ir.Temp.Virtual.t as t -> f t | #t -> ()

  include Comparable.Make (Args)
  include Hashable.Make (Args)
end
