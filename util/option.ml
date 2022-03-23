open Core

type 'a t = 'a option

module Lazy = struct
  let value ~default = function Some x -> x | None -> default ()
end
