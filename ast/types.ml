module type S = sig
  type integer
  type boolean
  type !'a vector
end

module Typed = struct
  type integer = int
  type boolean = bool
  type 'a vector = 'a array
end

module Untyped = struct
  type integer = unit
  type boolean = unit
  type 'a vector = 'a
end