open Type.Error

type id = string Node.Position.t

(** [carries_string ~f id] is an error with cause [f s], where [s] is
    the string corresponding to identifier [s], at location of [id] *)
let carries_string ~f id =
  let cause = f (Node.Position.get id) in
  Node.Position.error ~cause id

let bound = carries_string ~f:(fun s -> Bound s)
let expected_tau = Node.Position.error ~cause:ExpectedTau
let expected_fn = Node.Position.error ~cause:ExpectedFn
let fn_mismatch = carries_string ~f:(fun s -> FnMismatch s)
let unbound_intf = carries_string ~f:(fun s -> UnboundIntf s)
