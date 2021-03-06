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

  let num_caller_save = Sequence.length caller_save

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

  (** rsp, rbp, rip *)
  let num_unusable = 3

  let num_usable = Array.length reg_array - num_unusable

  let callee_save used =
    if used <= num_caller_save then []
    else Array.to_list (Array.slice reg_array num_caller_save used)

  let of_int_exn = Array.get reg_array

  let to_int_exn =
    let size = Array.length reg_array in
    let tbl = Table.create ~size () in
    Array.iteri reg_array ~f:(fun i reg ->
        Hashtbl.add_exn tbl ~key:reg ~data:i);
    Hashtbl.find_exn tbl

  let to_string = Variants.to_name

  include Comparable.Make (T)
end

module Bit8 = struct
  type t =
    [ `al
    | `bl
    | `cl
    | `dl
    | `spl
    | `bpl
    | `sil
    | `dil
    | `r8b
    | `r9b
    | `r10b
    | `r11b
    | `r12b
    | `r13b
    | `r14b
    | `r15b
    | `ip
    ]
  [@@deriving variants, equal, sexp, compare, hash]

  let to_64_bit = function
    | `al -> `rax
    | `bl -> `rbx
    | `cl -> `rcx
    | `dl -> `rdx
    | `spl -> `rsp
    | `bpl -> `rbp
    | `sil -> `rsi
    | `dil -> `rdi
    | `r8b -> `r8
    | `r9b -> `r9
    | `r10b -> `r10
    | `r11b -> `r11
    | `r12b -> `r12
    | `r13b -> `r13
    | `r14b -> `r14
    | `r15b -> `r15
    | `ip -> `rip

  let of_64_bit = function
    | `rax -> `al
    | `rbx -> `bl
    | `rcx -> `cl
    | `rdx -> `dl
    | `rsp -> `spl
    | `rbp -> `bpl
    | `rsi -> `sil
    | `rdi -> `dil
    | `r8 -> `r8b
    | `r9 -> `r9b
    | `r10 -> `r10b
    | `r11 -> `r11b
    | `r12 -> `r12b
    | `r13 -> `r13b
    | `r14 -> `r14b
    | `r15 -> `r15b
    | `rip -> `ip
end

module Args = struct
  type t =
    [ Bit64.t
    | Bit8.t
    ]
  [@@deriving equal, sexp, compare, hash]
end

include Args
include Hashable.Make (Args)

let to_64_bit = function
  | #Bit64.t as r -> r
  | #Bit8.t as r -> Bit8.to_64_bit r

let to_8_bit = function
  | #Bit64.t as r -> Bit8.of_64_bit r
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

  let is_concrete = function #Bit64.t -> true | _ -> false

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
