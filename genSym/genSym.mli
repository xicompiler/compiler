open Core

type 'a format = (int -> 'a, unit, string) Core.format
(** ['a format] is a format with exactly one integer format specifier*)

val create : ?init:int -> 'a format -> unit -> 'a
(** [create ?init fmt] is a symbol generator with a counter starting
    from [init], or 0 if not provided, that takes [()] and returns a
    fresh label with the current value of the counter replacing the
    integer format specificer in [fmt] *)
