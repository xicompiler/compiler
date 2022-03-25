open Core
open Subtype

type t = {
  gen_temp : unit -> string;
  gen_label : unit -> label;
  gen_global : unit -> label;
}
[@@deriving fields]

(** [temp_fmt] is the format of a temporary *)

let temp_fmt = format_of_string "t%d"

(** [label_fmt] is the format of labels, in accordance with the calling
    convention*)
let label_fmt = format_of_string "l%d"

let global_fmt = format_of_string "g%d"

let create () =
  {
    gen_temp = Gensym.create temp_fmt;
    gen_label = Gensym.create label_fmt;
    gen_global = Gensym.create global_fmt;
  }

(** [fresh2 ~fresh gen] is a pair of fresh values generated using
    [fresh] *)
let fresh2 ~fresh gen = Tuple2.map ~f:fresh (gen, gen)

(** [fresh3 ~fresh gen] is a triple of fresh values generated using
    [fresh] *)
let fresh3 ~fresh gen = Tuple3.map ~f:fresh (gen, gen, gen)

module Temp = struct
  let fresh { gen_temp } = `Temp (gen_temp ())

  let fresh2 = fresh2 ~fresh
  let fresh3 = fresh3 ~fresh
end

module Label = struct
  let generator = gen_label

  let fresh { gen_label } = gen_label ()

  let fresh2 = fresh2 ~fresh

  let fresh3 = fresh3 ~fresh
end

module Global = struct
  let generator = gen_global

  let fresh { gen_global } = gen_global ()

  let fresh2 = fresh2 ~fresh

  let fresh3 = fresh3 ~fresh
end
