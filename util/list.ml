open Core
open Result.Let_syntax

let rec fold2_result ~unequal_lengths ~f ~init l1 l2 =
  match (l1, l2) with
  | h1 :: t1, h2 :: t2 ->
      let%bind init = f init h1 h2 in
      fold2_result ~unequal_lengths ~f ~init t1 t2
  | [], [] -> Ok init
  | _ -> Error unequal_lengths

let hd_tl_exn = function
  | h :: t -> (h, t)
  | [] -> failwith "list empty"

(** Same as [rev_concat] but takes an accumulator *)
let rec rev_concat_acc acc = function
  | [] -> acc
  | (h :: t) :: t' -> rev_concat_acc (h :: acc) (t :: t')
  | [] :: t -> rev_concat_acc acc t

let rev_concat lst = rev_concat_acc [] lst
