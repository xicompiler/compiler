open Core
open Option.Let_syntax

type ('k, 'v, 'cmp) t = ('k, 'v, 'cmp) Map.t

let update_exn ?message m k ~f =
  Map.update m k ~f:(fun v -> f (Option.value_exn ?message v))

let pop_elt m =
  let%map k, v = Map.min_elt m in
  (k, v, Map.remove m k)
