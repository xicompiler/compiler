open Core

(** [Operand] represents an operand to an instruction *)
module Operand : module type of struct
  include Operand
end

(** [Generic] represents a generic assembly instruction, parameterized
    over operand types *)
module Generic : module type of struct
  include Generic
end

(** [Abstract] represents an abstract assembly instruction *)
module Abstract : module type of struct
  include Abstract
end

(** [ConditionCode] is a condition code in x86 *)
module ConditionCode : module type of struct
  include ConditionCode
end

(** [Output] represents the file functions needed for instruction
    selection and assembly code generation *)
module Output : sig
  val file_to_file :
    ?cache:Frontend.Check.cache ->
    src:string ->
    out:string ->
    deps:Frontend.Check.dependencies ->
    optimize:bool ->
    unit ->
    Frontend.Check.result File.Xi.result
  (** [file_to_file ~start lexbuf out] parses and typechecks the
      contents of file [src] and generates concrete asm to file [out] *)

  module Abstract : sig
    val file_to_file :
      ?cache:Frontend.Check.cache ->
      src:string ->
      out:string ->
      deps:Frontend.Check.dependencies ->
      optimize:bool ->
      unit ->
      Frontend.Check.result File.Xi.result
    (** [file_to_file ~start lexbuf out] parses and typechecks the
        contents of file [src] and generates abstract asm to file [out] *)
  end
end