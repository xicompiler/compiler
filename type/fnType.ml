open Core

type fn = {
  arg : Term.t;
  ret : Term.t;
}
[@@deriving sexp_of]

type t =
  [ `FnDecl of fn
  | `FnDefn of fn
  ]
[@@deriving sexp_of]

let make ?arg ?ret () =
  { arg = Term.or_unit arg; ret = Term.or_unit ret }

let decl ?arg ?ret () = `FnDecl (make ?arg ?ret ())
let defn ?arg ?ret () = `FnDefn (make ?arg ?ret ())

let matches fn1 fn2 =
  Term.equal fn1.arg fn2.arg && Term.equal fn1.ret fn2.ret
