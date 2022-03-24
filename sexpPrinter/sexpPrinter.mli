open Core

val print : Out_channel.t -> Sexp.t -> unit
(** [print out sexp] prints s-expression [sexp] to out channel [out],
    correctly handling escaped characters *)

val pp : Out_channel.t -> Sexp.t -> unit
(** [pp out sexp] prints s-expression [sexp] to out channel [out] using
    Jane Street Core's sexp pretty printer *)
