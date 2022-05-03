open Core

module Make (Set : Set.S) = struct
  let params ~use ~def =
    (* in[n] = use[n] ∪ (out[n] - def[n]) *)
    let f ~data ~vertex =
      let survivors = Set.diff data (def vertex) in
      Set.union (use vertex) survivors
    in
    Params.
      {
        f;
        meet = Set.union_list;
        top = Set.empty;
        direction = `Backward;
        equal = Set.equal;
      }
end
