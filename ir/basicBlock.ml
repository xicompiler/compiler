open Core
open Subtype

type t = Lir.stmt Fdeque.t

(** [break s1 s2] is [true] iff the boundary between [s1] and [s2] must
    be the boundary of a basic block *)
let break s1 s2 =
  match (s1, s2) with
  | (`Return _ | `Jump _ | `CJump _), _ | _, `Label _ -> true
  | _ -> false

let of_lir lir = lir |> List.group ~break |> List.map ~f:Fdeque.of_list
let first = Fdeque.peek_front
let last = Fdeque.peek_back

(** Same as [Fdeque.drop], but application to [Fdeque.empty] yields
    [Fdeque.empty] back*)
let drop t side =
  Option.value (Fdeque.drop t side) ~default:Fdeque.empty

(** [remove_first t] is [drop t `front] *)
let remove_first t = drop t `front

(** [remove_last t] is [drop t `back] *)
let remove_last t = drop t `back

let insert_first block ~stmt = Fdeque.enqueue_front block stmt
let set_first block = insert_first (remove_first block)
let insert_last block ~stmt = Fdeque.enqueue_back block stmt
let set_last block = insert_last (remove_last block)

let label block =
  match first block with
  | Some (`Label l) -> Some l
  | Some _ | None -> None

let has_label block ~label:l =
  match label block with Some l' -> String.equal l l' | None -> false

let insert_label block ~label = insert_first block ~stmt:(`Label label)
let to_list = Fdeque.to_list
