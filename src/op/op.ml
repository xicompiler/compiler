type binop = Binop.t [@@deriving hash, compare, sexp]

module Binop = Binop

type unop = Unop.t

module Unop = Unop
