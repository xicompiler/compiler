val allocate_instrs :
  offset:int Reg.Abstract.Table.t ->
  Operand.Abstract.t Generic.t list ->
  Operand.t Generic.t list
(** [allocate_fn ~offset instrs] is [instrs] with all of its variables
    concretized *)
