open Type.Error

type id = string Position.entry

(** [carries_string ~f id] is an error with cause [f s], where [s] is
    the string corresponding to identifier [s], at location of [id] *)
let carries_string ~f id =
  let cause = f (Entry.key id) in
  Position.Entry.error ~cause id

let bound = carries_string ~f:(fun s -> Bound s)
let unbound = carries_string ~f:(fun s -> Unbound s)
let expected_tau = Position.Entry.error ~cause:ExpectedTau
let expected_fn = Position.Entry.error ~cause:ExpectedFn
let expected_record = Position.Entry.error ~cause:ExpectedRecord
let fn_mismatch = carries_string ~f:(fun s -> FnMismatch s)
let record_mismatch = carries_string ~f:(fun s -> RecordMismatch s)
let unbound_intf = carries_string ~f:(fun s -> UnboundIntf s)
