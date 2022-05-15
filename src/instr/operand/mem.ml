open Core
open Option.Monad_infix
open Util.Fn

module Index = struct
  module Scale = struct
    type t = int64 [@@deriving sexp]

    let is_valid = function 1L | 2L | 4L | 8L -> true | _ -> false
    let to_string = Int64.to_string
  end

  type 'a t = {
    index : 'a;
    scale : Scale.t option;
  }
  [@@deriving fields, sexp]

  let create ?scale index = { index; scale }

  let to_string ~f { index; scale } =
    let index = f index in
    match scale with
    | None -> index
    | Some scale ->
        let scale = Scale.to_string scale in
        Printf.sprintf "%s * %s" index scale

  let map idx ~f = { idx with index = f idx.index }
  let with_index idx ~index = { idx with index }
end

module Size = struct
  type t =
    | Qword
    | Dword
    | Word
    | Byte
  [@@deriving variants, sexp]

  let to_string = Variants.to_name >> String.lowercase
end

type 'a generic = {
  segment : Ir.label option;
  size : Size.t;
  base : 'a;
  index : 'a Index.t option;
  offset : int64 option;
}
[@@deriving fields, sexp]

let create ?segment ?(size = Size.Qword) ?index ?offset base =
  { segment; size; base; index; offset }

let map mem ~f =
  { mem with base = f mem.base; index = mem.index >>| Index.map ~f }

type t = Reg.t generic

let string_of_mem ~f { segment; size; base; index; offset } =
  let size = Size.to_string size in
  let seg = Option.value segment ~default:"" in
  [ index >>| Index.to_string ~f; offset >>| Int64.to_string ]
  |> List.filter_opt
  |> List.cons (f base)
  |> String.concat ~sep:" + "
  |> Printf.sprintf "%s ptr %s[%s]" size seg

let to_string = string_of_mem ~f:Reg.to_string

module Abstract = struct
  type t = Reg.Abstract.t generic

  let to_string = string_of_mem ~f:Reg.Abstract.to_string
end

let with_registers ?index ~base mem = { mem with base; index }
