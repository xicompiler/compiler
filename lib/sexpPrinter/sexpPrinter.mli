open Core

val print : Out_channel.t -> Sexp.t -> unit
(** [print out sexp] prints s-expression [sexp] to out channel [out],
    correctly handling escaped characters *)
