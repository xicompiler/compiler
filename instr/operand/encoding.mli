open! Core

type ('reg, 'dest) rm = [ `RM of 'reg * 'dest ]
(** [('reg, 'dest) rm] is the type of a [RM] encoding, as described in
    the docs *)

type ('reg, 'dest) mr = [ `MR of 'reg * 'dest ]
(** [('reg, 'dest) mr] is the type of a [MR] encoding, as described in
    the docs *)

type 'dest mi = [ `MI of 'dest * Imm.t ]
(** ['dest mi] is the type of a [MI] encoding, as described in the docs *)

type ('reg, 'dest) rmi = [ `RMI of 'reg * 'dest * Imm.t ]
(** [('reg, 'dest) rmi] is the type of a [RMI] encoding, as described in
    the docs *)
