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

(** [reg ~fmt n] is the n[th] register with format [fmt] *)
let reg ~fmt n = `Temp (Printf.sprintf fmt n)

let rv = reg ~fmt:(format_of_string "_RV%d")
let rv1 = rv 1
let arg = reg ~fmt:(format_of_string "_ARG%d")

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

(** [Params] is the module type of parameters used to created a
    generated *)
module type Params = sig
  type sym

  val generator : t -> unit -> sym
end

(** [Gen] is the module type of a generated symbol *)
module type Gen = sig
  include Params

  val fresh : t -> sym
  val fresh2 : t -> sym * sym
  val fresh3 : t -> sym * sym * sym
end

module type LabelGen = Gen with type sym := label

module Make (Args : Params) = struct
  include Args

  let fresh gen = generator gen ()
  let fresh2 = fresh2 ~fresh
  let fresh3 = fresh3 ~fresh
end

module Label = Make (struct
  type sym = label

  let generator = gen_label
end)

module Global = Make (struct
  type sym = label

  let generator = gen_global
end)
