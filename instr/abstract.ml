open Core
open Operand
open Generic

type jmp =
  [ Dest.abstract
  | Ir.name
  ]

type mul =
  [ Dest.abstract
  | (Reg.abstract, Dest.abstract) Encoding.rm
  | (Reg.abstract, Dest.abstract) Encoding.rmi
  ]

type t =
  < reg : Reg.abstract
  ; reg8 : Reg.Bit8.abstract
  ; dest : Dest.abstract
  ; operand : abstract
  ; jmp : jmp
  ; mul : mul >
  Generic.t

(** [zero e] zeroes the contents of [e] *)
let zero e : t = Xor (e, (e :> abstract))

module Expr = struct
  type translation = t list * string

  let rev_munch_name ~init ~gensym l : translation =
    let t = gensym () in
    let dst = `Temp t in
    let mem = Mem.create ~segment:l dst in
    (Mov (dst, `Mem mem) :: init, t)

  let rev_munch_const ~init ~gensym i : translation =
    let t = gensym () in
    (Mov (`Temp t, `Imm i) :: init, t)

  let rec rev_munch ~init ~gensym : Ir.Lir.expr -> translation =
    function
    | `Name l -> rev_munch_name ~init ~gensym l
    | `Const i -> rev_munch_const ~init ~gensym i
    | `Bop (op, e1, e2) -> rev_munch_bop ~init ~gensym op e1 e2
    | `Temp t -> (init, t)
    | `Mem e -> rev_munch_mem ~init ~gensym e

  and rev_munch2 ~init ~gensym e1 e2 =
    let s1, t1 = rev_munch ~init ~gensym e1 in
    let s2, t2 = rev_munch ~init:s1 ~gensym e2 in
    (s2, t1, t2)

  and rev_munch_mem ~init ~gensym e : translation =
    (* TODO : better tiling *)
    let s, t = rev_munch ~init ~gensym e in
    let tmp = `Temp t in
    let mem = Mem.create tmp in
    (Mov (tmp, `Mem mem) :: s, t)

  and rev_munch_bop ~init ~gensym op e1 e2 =
    match op with
    | `Add -> rev_munch_add ~init ~gensym e1 e2
    | `And -> rev_munch_and ~init ~gensym e1 e2
    | `Div -> rev_munch_div ~init ~gensym e1 e2
    | `HMul -> rev_munch_hmul ~init ~gensym e1 e2
    | `Mod -> rev_munch_mod ~init ~gensym e1 e2
    | `Mul -> rev_munch_mul ~init ~gensym e1 e2
    | `Or -> rev_munch_or ~init ~gensym e1 e2
    | `Sub -> rev_munch_sub ~init ~gensym e1 e2
    | `Xor -> rev_munch_xor ~init ~gensym e1 e2
    | #Ir.Op.cmp as op -> rev_munch_cmp ~init ~gensym op e1 e2

  and rev_munch_add ~init ~gensym e1 e2 =
    let s, t1, t2 = rev_munch2 ~init ~gensym e1 e2 in
    let index = Mem.Index.create (`Temp t2) in
    let sum = Mem.create (`Temp t1) ~index in
    let t3 = gensym () in
    (Lea (`Temp t3, sum) :: s, t3)

  and rev_munch_and ~init ~gensym e1 e2 =
    let s, t1, t2 = rev_munch2 ~init ~gensym e1 e2 in
    (And (`Temp t1, `Temp t2) :: s, t1)

  and rev_munch_div ~init ~gensym e1 e2 =
    let s, t1, t2 = rev_munch2 ~init ~gensym e1 e2 in
    let tmp1 = `Temp t1 in
    ( Mov (tmp1, `rax)
      :: IDiv (`Temp t2)
      :: zero `rax
      :: Mov (`rdx, tmp1)
      :: s,
      t1 )

  and rev_munch_hmul ~init ~gensym e1 e2 =
    let s, t1, t2 = rev_munch2 ~init ~gensym e1 e2 in
    let tmp1 = `Temp t1 in
    ( Mov (tmp1, `rdx)
      :: IMul (`Temp t2)
      :: Mov (`rax, tmp1)
      :: s,
      t1 )

  and rev_munch_mod ~init ~gensym e1 e2 =
    let s, t1, t2 = rev_munch2 ~init ~gensym e1 e2 in
    let tmp1 = `Temp t1 in
    ( Mov (tmp1, `rdx)
      :: IDiv (`Temp t2)
      :: zero `rax
      :: Mov (`rdx, tmp1)
      :: s,
      t1 )

  and rev_munch_mul ~init ~gensym e1 e2 =
    let s, t1, t2 = rev_munch2 ~init ~gensym e1 e2 in
    (IMul (`RM (`Temp t1, `Temp t2)) :: s, t1)

  and rev_munch_or ~init ~gensym e1 e2 =
    let s, t1, t2 = rev_munch2 ~init ~gensym e1 e2 in
    (Or (`Temp t1, `Temp t2) :: s, t1)

  and rev_munch_sub ~init ~gensym e1 e2 =
    let s, t1, t2 = rev_munch2 ~init ~gensym e1 e2 in
    (Sub (`Temp t1, `Temp t2) :: s, t1)

  and rev_munch_xor ~init ~gensym e1 e2 =
    let s, t1, t2 = rev_munch2 ~init ~gensym e1 e2 in
    (Xor (`Temp t1, `Temp t2) :: s, t1)

  and rev_munch_cmp ~init ~gensym op e1 e2 =
    let s, t1, t2 = rev_munch2 ~init ~gensym e1 e2 in
    let t3 = gensym () in
    let cc = ConditionCode.of_cmp op in
    (Setcc (cc, `Temp t3) :: Cmp (`Temp t1, `Temp t2) :: s, t3)

  let munch ~gensym e =
    e |> rev_munch ~init:[] ~gensym |> Tuple2.map_fst ~f:List.rev
end

module Stmt = struct
  type translation = t list

  (** [rev_munch_jump_general ~init:\[s1; ...; sn\] ~gensym e] is
      [\[Jmp t; sm; ...; s1; ...; sn\]] where [t] is a fresh temporary
      and [sm; ...; sm-1] effect the move of the translation of [e] into
      [t] *)
  let rev_munch_jump_general ~init ~gensym e : translation =
    let s, t = Expr.rev_munch ~init ~gensym e in
    Jmp (`Temp t) :: s

  (** [rev_munch_jump  ~init:\[s1; ...; sn\] ~gensym e] is the
      translation of the lowered IR statement [`Jump e], followed by
      [init] *)
  let rev_munch_jump ~init ~gensym : Ir.Lir.expr -> translation =
    function
    | `Name _ as name -> Jmp name :: init
    | e -> rev_munch_jump_general ~init ~gensym e

  (** [rev_munch_jcc ~init:\[s1; ...; sn\] ~gensym l op e1 e2] is the
      translation of the lowered, reordered IR statement
      [`CJump (op (e1, e2), l)] in reverse order followed by [init] *)
  let rev_munch_jcc ~init ~gensym l op e1 e2 : translation =
    (* TODO : take advantage of memory, immediate operands in cmp *)
    let s, t1, t2 = Expr.rev_munch2 ~init ~gensym e1 e2 in
    let cc = ConditionCode.of_cmp op in
    Jcc (cc, l) :: Cmp (`Temp t1, `Temp t2) :: s

  (** [rev_munch_cjump ~init:\[s1; ...; sn\] ~gensym e l] is the
      translation of the lowered, reordered IR statement [`CJump (e, l)]
      in reverse order followed by [init]. The most general translation
      is used, i.e. [test t, t] followed by [jnz t], where [t] is a
      temporary holding the translation of [e] *)
  let rev_munch_cjump_general ~init ~gensym e l : translation =
    (* TODO : take advantage of memory, immediate operands in test *)
    let s, t = Expr.rev_munch ~init ~gensym e in
    let tmp = `Temp t in
    jnz l :: Test (tmp, tmp) :: s

  (** [rev_munch_cjump ~init:\[s1; ...; sn\] ~gensym e l] is the
      translation of the lowered, reordered IR statement [`CJump (e, l)]
      in reverse order followed by [init]. If possible, an optimtized
      translation is produced, levering [Jcc] *)
  let rev_munch_cjump ~init ~gensym e l : translation =
    match e with
    | `Bop ((#Ir.Op.cmp as op), e1, e2) ->
        rev_munch_jcc ~init ~gensym l op e1 e2
    | #Ir.Lir.expr -> rev_munch_cjump_general ~init ~gensym e l

  (** [rev_munch_label ~init l] is the translation of [`Label l], in
      reverse order, followed by [init] *)
  let rev_munch_label ~init l = failwith "unimplemented"

  (** [rev_munch_call ~init ~gensym i e es] is the translation of
      [`Call (i, e, es)], in reverse order, followed by [init] *)
  let rev_munch_call ~init ~gensym i e es = failwith "unimplemented"

  (** [rev_munch_move ~init ~gensym e1 e2] is the translation of
      [`Move (e1, e2)], in reverse order, followed by [init] *)
  let rev_munch_move ~init ~gensym e1 e2 =
    (* TODO : eliminate uneeded temps *)
    match e1 with
    | `Mem addr ->
        let s, addr, t2 = Expr.rev_munch2 ~init ~gensym addr e2 in
        let mem = Mem.create (`Temp addr) in
        Mov (`Mem mem, `Temp t2) :: s
    | `Temp _ as t1 ->
        let s, t2 = Expr.rev_munch ~init ~gensym e2 in
        Mov (t1, `Temp t2) :: s

  (** [rev_munch_return ~init ~gensym es] is the translation of
      [`Return es], in reverse order, followed by [init] *)
  let rev_munch_return ~init ~gensym es = failwith "unimplemented"

  (** [rev_munch ~init ~gensym e] is the translation of [e], in reverse
      order, followed by [init] *)
  let rev_munch ~init ~gensym : Ir.Reorder.stmt -> translation =
    function
    | `Jump e -> rev_munch_jump ~init ~gensym e
    | `CJump (e, l) -> rev_munch_cjump ~init ~gensym e l
    | `Label l -> rev_munch_label ~init l
    | `Call (i, e, es) -> rev_munch_call ~init ~gensym i e es
    | `Move (e1, e2) -> rev_munch_move ~init ~gensym e1 e2
    | `Return es -> rev_munch_return ~init ~gensym es

  let munch ~gensym s = failwith "unimplemented"
end
