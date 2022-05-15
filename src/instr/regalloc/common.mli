val load :
  offset:int Reg.Abstract.Table.t ->
  src:Reg.Abstract.t ->
  [> Reg.Bit64.t ] ->
  Concrete.t

val store :
  offset:int Reg.Abstract.Table.t ->
  dst:Reg.Abstract.t ->
  [> Reg.Bit64.t ] ->
  Concrete.t
