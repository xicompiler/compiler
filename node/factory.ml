open Core

module Make (B : Abstract.Base) = struct
  include B

  type 'a t = 'a B.t

  let compose ~f = Fn.compose f get
  let map ~f node = set ~value:(compose ~f node) node
end
