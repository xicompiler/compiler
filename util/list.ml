open Core
open Result.Let_syntax

let rec fold2_result ~unequal_lengths ~f ~init l1 l2 =
  match (l1, l2) with
  | h1 :: t1, h2 :: t2 ->
      let%bind init = f init h1 h2 in
      fold2_result ~unequal_lengths ~f ~init t1 t2
  | [], [] -> Ok init
  | _ -> Error unequal_lengths
