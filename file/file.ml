include Main
module Xi = Xi

module type Diagnostic = sig
  val file_to_file : src:string -> out:string -> unit Xi.result
end
