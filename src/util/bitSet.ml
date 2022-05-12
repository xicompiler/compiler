open Core
open Option.Monad_infix

type t = int

let empty = 0

(* -1 is the bit vector of all 1's *)
let universe = -1
let is_empty = ( = ) empty
let inter = ( land )
let union = ( lor )
let complement = lnot

(** [max_elt] is the maximum element able to be held in a bitset *)
let max_elt = pred Int.num_bits

let check_bounds i ~max =
  if i < 0 || i > max then invalid_arg "out of bounds"

let singleton i =
  check_bounds i ~max:max_elt;
  Int.shift_left 1 i

let add i s = union s (singleton i)
let diff s1 s2 = inter s1 (complement s2)
let remove i s = diff s (singleton i)

let range ~max =
  check_bounds max ~max:Int.num_bits;
  pred (Int.shift_left 1 max)

(** [log2 i] is the logarithm base 2 of the unsigned interpretation of
    [i] *)
let log2 i = if i < 0 then max_elt else Int.floor_log2 i

let pop_min s =
  if is_empty s then None
  else
    (* Here, s & ~(s - 1) zeroes all bits in s but the least significant
       nonzero bit. The resulting integer represents the singleton set
       containing only the minimum element *)
    let singleton = s land lnot (pred s) in
    (* Taking the log2 of this number gets its index in the set *)
    Some (log2 singleton, diff s singleton)

let min s = pop_min s >>| fst

let elements s =
  let rec aux acc s =
    match pop_min s with
    | Some (min, t) -> aux (min :: acc) t
    | None -> acc
  in
  aux [] s
