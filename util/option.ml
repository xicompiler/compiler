open Core

module Lazy = struct
  let value ~default = function Some x -> x | None -> default ()
end
