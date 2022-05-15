open Core

type record = (string * Tau.t) list [@@deriving sexp_of]

type t =
  [ `RecordDecl of record
  | `RecordDefn of record
  ]
[@@deriving sexp_of]

let make ?fields () = Option.value ~default:[] fields
let decl ?fields () = `RecordDecl (make ?fields ())
let defn ?fields () = `RecordDefn (make ?fields ())

(** [fields_match f1 f2] is true if the name and type of field [f1]
    matches that of [f2], and false otherwise *)
let fields_match f1 f2 =
  match (f1, f2) with
  | (s1, t1), (s2, t2) -> String.equal s1 s2 && Tau.equal t1 t2

let rec matches r1 r2 =
  match (r1, r2) with
  | [], _ | _, [] -> true
  | h1 :: t1, h2 :: t2 ->
      if fields_match h1 h2 then matches t1 t2 else false
