open Core
open Option.Monad_infix
open Util.Fn

module Index = struct
  type scale =
    [ `One
    | `Two
    | `Four
    | `Eight
    ]

  (** [int_of_scale scale] is the integer representation of [scale] *)
  let int_of_scale = function
    | `One -> 1
    | `Two -> 2
    | `Four -> 4
    | `Eight -> 8

  (** [string_of_scale scale] is the string representation of [scale] *)
  let string_of_scale = int_of_scale >> string_of_int

  type 'a t = {
    index : 'a;
    scale : scale option;
  }
  [@@deriving fields]

  let create ?scale index = { index; scale }

  let to_string ~f { index; scale } =
    let index = f index in
    match scale with
    | None -> index
    | Some scale ->
        let scale = string_of_scale scale in
        Printf.sprintf "%s * %s" index scale

  let with_index idx ~index = { idx with index }
end

module Size = struct
  type t =
    | Qword
    | Dword
    | Word
    | Byte
  [@@deriving variants]

  let to_string = Variants.to_name >> String.lowercase
end

type 'a generic = {
  segment : Ir.label option;
  size : Size.t;
  base : 'a;
  index : 'a Index.t option;
  offset : int64 option;
}
[@@deriving fields]

let create ?segment ?(size = Size.Qword) ?index ?offset base =
  { segment; size; base; index; offset }

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
