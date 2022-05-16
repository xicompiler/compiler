open! Core

type t = Operand.t Generic.t

module Asm = struct
  type t = Operand.t Generic.Asm.t

  let to_string = Generic.Asm.to_string ~f:Operand.to_string
end

let to_string = Generic.to_string ~f:Operand.to_string
