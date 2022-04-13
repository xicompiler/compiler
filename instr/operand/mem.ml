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

  let create ?scale index = { index; scale }

  let to_string { index; scale } =
    let index = Reg.to_string index in
    match scale with
    | None -> index
    | Some scale ->
        let scale = string_of_scale scale in
        Printf.sprintf "%s * %s" index scale
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

let create ?segment ?(size = Size.Qword) ?index ?offset base =
  { segment; size; base; index; offset }

type t = Reg.t generic

let to_string { segment; size; base; index; offset } =
  let size = Size.to_string size in
  let seg = Option.value segment ~default:"" in
  [ index >>| Index.to_string; offset >>| Int64.to_string ]
  |> List.filter_opt
  |> List.cons (Reg.to_string base)
  |> String.concat ~sep:" + "
  |> Printf.sprintf "%s ptr %s[%s]" size seg

type abstract = Reg.abstract generic
