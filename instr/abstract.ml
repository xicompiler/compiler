open Core
open Generic
open Util.Fn

type t = Operand.Abstract.t Generic.t

(** [Expr] contains functions for manipulating IR expressions and
    translating them into abstract assembly *)
module Expr = struct
  type translation = t list * string
  (** A [translation] is a pair [(s, t)] where [s] is a sequence of
      abstract assembly instructions necessary to effect the movement of
      the translation of an expression into temporary [t] *)

  (** [rev_munch_name ~init  l] is the translation of the lowered,
      reordered IR statement [`Name l], followed by init *)
  let rev_munch_name ~init ~t l =
    let mem = Mem.create ~segment:l `rip in
    Lea (`Temp t, `Mem mem) :: init

  (** [rev_munch_const ~init ~gensym i] is the translation of the
      lowered, reordered IR statement [`Const i], followed by init *)
  let rev_munch_const ~init ~t i = Mov (`Temp t, `Imm i) :: init

  (** [rev_munch_virtual ~init ~gensym v] is the translation of moving a
      virtual arg or register into a fresh temp, followed by init *)
  let rev_munch_move ~init ~t v = Mov (`Temp t, v) :: init

  let rec rev_munch ~t ~init ~gensym = function
    | `Name l -> rev_munch_name ~init ~t l
    | `Const i -> rev_munch_const ~init ~t i
    | `Bop (op, e1, e2) -> rev_munch_bop ~init ~gensym op e1 e2
    | `Mem e -> rev_munch_mem ~t ~init ~gensym e
    | #Ir.Temp.Virtual.t as t' -> rev_munch_move ~t ~init t'

  and rev_munch_immut ~init ~gensym = function
    | `Temp t -> (init, t)
    | e -> 
        let t = gensym () in 
        (rev_munch ~t ~init ~gensym e, t)

  and rev_munch2_immut ~init ~gensym e1 e2 =
    let s1, t1 = rev_munch_immut ~init ~gensym e1 in
    let s2, t2 = rev_munch_immut ~init:s1 ~gensym e2 in
    (s1, t1, t2)

  and rev_munch_index ?offset ?scale ~init ~gensym ~index base =
    let s, base, index = rev_munch2_immut ~init ~gensym base index in
    let index = Mem.Index.create ?scale (`Temp index) in
    (s, Mem.create ~index ?offset (`Temp base))

  and rev_munch_base ~init ~gensym ?offset base =
    let s, base = rev_munch_immut ~init ~gensym base in
    (s, Mem.create ?offset (`Temp base))

  and rev_munch_mem_sum ~init ~gensym e1 e2 =
    let open Mem.Index in
    match (e1, e2) with
    | base, `Bop (`Add, `Bop (`Mul, idx, `Const scale), `Const off)
      when Scale.is_valid scale ->
        rev_munch_index ~offset:off ~scale ~init ~gensym ~index:idx base
    | base, `Bop (`Add, `Bop (`Mul, `Const scale, idx), `Const off)
      when Scale.is_valid scale ->
        rev_munch_index ~offset:off ~scale ~init ~gensym ~index:idx base
    | base, `Bop (`Add, `Const off, `Bop (`Mul, idx, `Const scale))
      when Scale.is_valid scale ->
        rev_munch_index ~offset:off ~scale ~init ~gensym ~index:idx base
    | base, `Bop (`Add, `Const off, `Bop (`Mul, `Const scale, idx))
      when Scale.is_valid scale ->
        rev_munch_index ~offset:off ~scale ~init ~gensym ~index:idx base
    | `Bop (`Add, `Bop (`Mul, idx, `Const scale), `Const off), base
      when Scale.is_valid scale ->
        rev_munch_index ~offset:off ~scale ~init ~gensym ~index:idx base
    | `Bop (`Add, `Bop (`Mul, `Const scale, idx), `Const off), base
      when Scale.is_valid scale ->
        rev_munch_index ~offset:off ~scale ~init ~gensym ~index:idx base
    | `Bop (`Add, `Const off, `Bop (`Mul, idx, `Const scale)), base
      when Scale.is_valid scale ->
        rev_munch_index ~offset:off ~scale ~init ~gensym ~index:idx base
    | `Bop (`Add, `Const off, `Bop (`Mul, `Const scale, idx)), base
      when Scale.is_valid scale ->
        rev_munch_index ~offset:off ~scale ~init ~gensym ~index:idx base
    | `Const off, `Bop (`Add, `Bop (`Mul, idx, `Const scale), base)
      when Scale.is_valid scale ->
        rev_munch_index ~offset:off ~scale ~init ~gensym ~index:idx base
    | `Const off, `Bop (`Add, `Bop (`Mul, `Const scale, idx), base)
      when Scale.is_valid scale ->
        rev_munch_index ~offset:off ~scale ~init ~gensym ~index:idx base
    | `Const off, `Bop (`Add, base, `Bop (`Mul, idx, `Const scale))
      when Scale.is_valid scale ->
        rev_munch_index ~offset:off ~scale ~init ~gensym ~index:idx base
    | `Const off, `Bop (`Add, base, `Bop (`Mul, `Const scale, idx))
      when Scale.is_valid scale ->
        rev_munch_index ~offset:off ~scale ~init ~gensym ~index:idx base
    | `Bop (`Add, `Bop (`Mul, idx, `Const scale), base), `Const off
      when Scale.is_valid scale ->
        rev_munch_index ~offset:off ~scale ~init ~gensym ~index:idx base
    | `Bop (`Add, `Bop (`Mul, `Const scale, idx), base), `Const off
      when Scale.is_valid scale ->
        rev_munch_index ~offset:off ~scale ~init ~gensym ~index:idx base
    | `Bop (`Add, base, `Bop (`Mul, idx, `Const scale)), `Const off
      when Scale.is_valid scale ->
        rev_munch_index ~offset:off ~scale ~init ~gensym ~index:idx base
    | `Bop (`Add, base, `Bop (`Mul, `Const scale, idx)), `Const off
      when Scale.is_valid scale ->
        rev_munch_index ~offset:off ~scale ~init ~gensym ~index:idx base
    | base, `Bop (`Mul, idx, `Const scale) when Scale.is_valid scale ->
        rev_munch_index ~scale ~init ~gensym ~index:idx base
    | base, `Bop (`Mul, `Const scale, idx) when Scale.is_valid scale ->
        rev_munch_index ~scale ~init ~gensym ~index:idx base
    | `Bop (`Mul, idx, `Const scale), base when Scale.is_valid scale ->
        rev_munch_index ~scale ~init ~gensym ~index:idx base
    | `Bop (`Mul, `Const scale, idx), base when Scale.is_valid scale ->
        rev_munch_index ~scale ~init ~gensym ~index:idx base
    | base, `Bop (`Add, idx, `Const off)
    | base, `Bop (`Add, `Const off, idx)
    | `Bop (`Add, idx, `Const off), base
    | `Bop (`Add, `Const off, idx), base ->
        rev_munch_index ~gensym ~init ~offset:off ~index:idx base
    | base, `Const off | `Const off, base ->
        rev_munch_base ~init ~gensym ~offset:off base
    | _, _ -> rev_munch_index ~init ~gensym ~index:e2 e1

  (** [rev_munch2 ~init ~gensym e1 e2] is [(s, t1, t2)], where [s] is
      the reverse sequence of instructions to move [e1] into [t1], then
      [e2] into [t2] *)
  and rev_munch2 ~init ~gensym ~t1 ~t2 e1 e2 =
    let s = rev_munch ~t:t1 ~init ~gensym e1 in
    rev_munch ~t:t2 ~init:s ~gensym e2

  and rev_munch_operand ~init ~gensym = function
    | `Mem addr ->
        let s, mem = rev_munch_addr ~init ~gensym addr in
        (s, `Mem mem)
    | e ->
        let s, t = rev_munch_immut ~init ~gensym e in
        (s, `Temp t)

  and rev_munch_rmi ~init ~gensym = function
    | `Const i -> (init, `Imm i)
    | e -> rev_munch_operand ~init ~gensym e

  and rev_munch_ri ~init ~gensym = function
    | `Const i -> (init, `Imm i)
    | e ->
        let s, t = rev_munch_immut ~init ~gensym e in
        (s, `Temp t)

  (** [rev_munch2_map ~f ~init:\[sn; ...; s1\] ~gensym e1 e2] is
      [\[f t1 t2; si; ...; sj; ...; sn; ... s1\]] where
      [\[sj; ...; sn+1\]] move the translation of [e1] into [t1] in
      reverse order and [si; ...; sj+1] move the translation of [e2]
      into [t2] in reverse order *)
  and rev_munch2_map ~f ~t ~init ~gensym e1 e2 =
    let s1 = rev_munch ~t ~init ~gensym e1 in
    let s2, e2 = rev_munch_operand ~init:s1 ~gensym e2 in
    f (`Temp t) e2 :: s2

  and rev_munch_addr ~init ~gensym = function
    | `Bop (`Add, e1, e2) -> rev_munch_mem_sum ~init ~gensym e1 e2
    | e ->
        let s, t = rev_munch_immut ~init ~gensym e in
        (s, Mem.create (`Temp t))

  (** [rev_munch_mem ~init ~gensym e] is the translation of the lowered,
      reordered IR statement [`Mem e], followed by init *)
  and rev_munch_mem ~t ~init ~gensym e =
    (* TODO : better tiling *)
    let s, mem = rev_munch_addr ~init ~gensym e in
    Mov (`Temp t, `Mem mem) :: s

  (** [rev_munch_bop ~init ~gensym op e1 e2] is the translation of the
      lowered, reordered IR statement [`Bop (op, e1, e2)], followed by
      init *)
  and rev_munch_bop ~t ~init ~gensym : t list = function
    | `Add -> rev_munch_add ~t ~init ~gensym
    | `And -> rev_munch_and ~t ~init ~gensym
    | `Div -> rev_munch_div ~t ~init ~gensym
    | `HMul -> rev_munch_hmul ~t ~init ~gensym
    | `Mod -> rev_munch_mod ~t ~init ~gensym
    | `Mul -> rev_munch_mul ~t ~init ~gensym
    | `Or -> rev_munch_or ~t ~init ~gensym
    | `Sub -> rev_munch_sub ~t ~init ~gensym
    | `Xor -> rev_munch_xor ~t ~init ~gensym
    | #Ir.Op.cmp as op -> rev_munch_cmp ~t ~init ~gensym op

  (** [rev_munch_add ~init ~gensym e1 e2] is the translation of the
      lowered, reordered IR statement [`Bop (`Add, e1, e2)], followed by
      init *)
  and rev_munch_add ~t ~init ~gensym e1 e2 =
    let s, sum = rev_munch_mem_sum ~init ~gensym e1 e2 in
    Lea (`Temp t, `Mem sum) :: s

  (** [rev_munch_and ~init ~gensym e1 e2] is the translation of the
      lowered, reordered IR statement [`Bop (`And, e1, e2)], followed by
      init *)
  and rev_munch_and ~t ~init = rev_munch2_map ~f:and_ ~init

  (** [rev_munch_div ~init ~gensym e1 e2] is the translation of the
      lowered, reordered IR statement [`Bop (`Div, e1, e2)], followed by
      init *)
  and rev_munch_div ~init ~gensym e1 e2 =
    let s, t1, t2 = rev_munch2 ~init ~gensym e1 e2 in
    let tmp1 = `Temp t1 in
    ( Mov (tmp1, `rax)
      :: IDiv (`Temp t2)
      :: zero `rdx
      :: Mov (`rax, tmp1)
      :: s,
      t1 )

  (** [rev_munch_hmul ~init ~gensym e1 e2] is the translation of the
      lowered, reordered IR statement [`Bop (`HMul, e1, e2)], followed
      by init *)
  and rev_munch_hmul ~init ~gensym e1 e2 =
    let s, t1, t2 = rev_munch2 ~init ~gensym e1 e2 in
    let tmp1 = `Temp t1 in
    let multiplicand = `M (`Temp t2) in
    (Mov (tmp1, `rdx) :: IMul multiplicand :: Mov (`rax, tmp1) :: s, t1)

  (** [rev_munch_mod ~init ~gensym e1 e2] is the translation of the
      lowered, reordered IR statement [`Bop (`Mod, e1, e2)], followed by
      init *)
  and rev_munch_mod ~init ~gensym e1 e2 =
    let s, t1, t2 = rev_munch2 ~init ~gensym e1 e2 in
    let tmp1 = `Temp t1 in
    ( Mov (tmp1, `rdx)
      :: IDiv (`Temp t2)
      :: zero `rdx
      :: Mov (`rax, tmp1)
      :: s,
      t1 )

  (** [rev_munch_mul ~init ~gensym e1 e2] is the translation of the
      lowered, reordered IR statement [`Bop (`Mul, e1, e2)], followed by
      init *)
  and rev_munch_mul ~init =
    let f t1 t2 = IMul (`RM (t1, t2)) in
    rev_munch2_map ~f ~init

  (** [rev_munch_or ~init ~gensym e1 e2] is the translation of the
      lowered, reordered IR statement [`Bop (`Or, e1, e2)], followed by
      init *)
  and rev_munch_or ~init = rev_munch2_map ~f:or_ ~init

  (** [rev_munch_sub ~init ~gensym e1 e2] is the translation of the
      lowered, reordered IR statement [`Bop (`Sub, e1, e2)], followed by
      init *)
  and rev_munch_sub ~init = rev_munch2_map ~f:sub ~init

  (** [rev_munch_xor ~init ~gensym e1 e2] is the translation of the
      lowered, reordered IR statement [`Bop (`Xors, e1, e2)], followed
      by init *)
  and rev_munch_xor ~init = rev_munch2_map ~f:xor ~init

  (** [rev_munch_cmp ~init ~gensym e1 e2] is the translation of the
      lowered, reordered IR statement [`Bop (cmp, e1, e2)], where [cmp]
      is a comparison operator, followed by init *)
  and rev_munch_cmp ~init ~gensym op e1 e2 =
    let s, t1, t2 = rev_munch2 ~init ~gensym e1 e2 in
    let t3 = gensym () in
    let cc = ConditionCode.of_cmp op in
    ( Movzx (`Temp t3, `Temp t3)
      :: Setcc (cc, `Temp t3)
      :: Cmp (`Temp t1, `Temp t2)
      :: s,
      t3 )

  (** [rev_munch_list ~init:\[si; ...; sj\] ~gensym \[e1; ...; en\]] is
      a pair [(\[sj; ...; si; ...; s1\], \[tn; ...; t1\])] where
      [\[s1; ...; si-1\]] effects the moves of the translation of each
      [ei] into [ti] *)
  let rev_munch_list ~init ~gensym =
    let f (init, ts) e =
      let s, t = rev_munch ~init ~gensym e in
      (s, t :: ts)
    in
    List.fold ~init:(init, []) ~f

  (** [rev_munch_label ~init ~gensym e] is [(init, `Name l)] if [e] is
      [`Name l] and [(s, `Temp t)] where [(s, t)] is
      [rev_munch ~init ~gensym e] otherwise *)
  let rev_munch_label ~init ~gensym = function
    | `Name _ as name -> (init, name)
    | e ->
        let s, t = rev_munch ~init ~gensym e in
        (s, `Temp t)

  (** [munch ~gensym e] is a pair [(s, t)] where [s] is a sequence of
      abstract assembly instructions necessary to effect the movement of
      the translation of [e] into temporary [t] *)
  let munch ~gensym e =
    e |> rev_munch ~init:[] ~gensym |> Tuple2.map_fst ~f:List.rev
end

(** [Stmt] contains functions for manipulating IR statements and
    translating them into abstract assembly *)
module Stmt = struct
  type translation = t list
  (** A [translation] is a list of abstract assembly instruction *)

  (** [rev_munch_jump  ~init:\[s1; ...; sn\] ~gensym e] is the
      translation of the lowered, reordered IR statement [`Jump e],
      followed by [init] *)
  let rev_munch_jump ~init ~gensym e : translation =
    let s, e = Expr.rev_munch_label ~init ~gensym e in
    Jmp e :: s

  (** [rev_munch_jcc ~init:\[s1; ...; sn\] ~gensym l op e1 e2] is the
      translation of the lowered, reordered IR statement
      [`CJump (op (e1, e2), l)], in reverse order, followed by [init] *)
  let rev_munch_jcc ~init ~gensym l op e1 e2 : translation =
    (* TODO : take advantage of memory, immediate operands in cmp *)
    let s, t1, t2 = Expr.rev_munch2 ~init ~gensym e1 e2 in
    let cc = ConditionCode.of_cmp op in
    Jcc (cc, l) :: Cmp (`Temp t1, `Temp t2) :: s

  (** [rev_munch_cjump ~init:\[s1; ...; sn\] ~gensym e l] is the
      translation of the lowered, reordered IR statement
      [`CJump (e, l)], in reverse order, followed by [init]. The most
      general translation is used, i.e. [test t, t] followed by [jnz t],
      where [t] is a temporary holding the translation of [e] *)
  let rev_munch_cjump_general ~init ~gensym e l : translation =
    (* TODO : take advantage of memory, immediate operands in test *)
    let s, t = Expr.rev_munch ~init ~gensym e in
    let tmp = `Temp t in
    jnz l :: Test (tmp, tmp) :: s

  (** [rev_munch_cjump ~init:\[s1; ...; sn\] ~gensym e l] is the
      translation of the lowered, reordered IR statement
      [`CJump (e, l)], in reverse order, followed by [init]. If
      possible, an optimtized translation is produced, levering [Jcc] *)
  let rev_munch_cjump ~init ~gensym e l : translation =
    match e with
    | `Bop ((#Ir.Op.cmp as op), e1, e2) ->
        rev_munch_jcc ~init ~gensym l op e1 e2
    | #Ir.Lir.expr -> rev_munch_cjump_general ~init ~gensym e l

  (** [rev_munch_label ~init l] is the translation of [`Label l], in
      reverse order, followed by [init] *)
  let rev_munch_label ~init l = Label l :: init

  (** [pass_args t i] moves temp [t] into arg register [i], or pushes it
      onto the stack *)
  let pass_args t = function
    | 1 -> Mov (`rdi, t)
    | 2 -> Mov (`rsi, t)
    | 3 -> Mov (`rdx, t)
    | 4 -> Mov (`rcx, t)
    | 5 -> Mov (`r8, t)
    | 6 -> Mov (`r9, t)
    | _ -> Push t

  (** [align] is an instruction that forces the stack pointer to be
      16-byte aligned *)
  let align : t = And (`rsp, Imm.of_int ~-16)

  (** [rev_alloc_ret ~init m] is a sequence of instructions that
      allocates enought stack size for [m - 2] return values and then
      moves the address of this stack segment into [`rdi], all in
      reverse order. Requires: [m > 2] *)
  let rev_alloc_ret ~init m : translation =
    let seg_size = 8 * (m - 2) in
    Mov (`rdi, `rsp) :: Sub (`rsp, Imm.of_int seg_size) :: init

  (** [rev_dealloc_args ~init ~n ~m es] is a sequence of instructions
      that frees any stack space used to store the translations of
      arguments [es], in reverse order, followed by [init] *)
  let rev_dealloc_args ~init ~n ~m es : translation =
    let on_stack = if m <= 2 then 6 else 5 in
    (* if [n <= on_stack] args, none were pushed to stack *)
    if n <= on_stack then init
    else
      (* stack space occupied by args *)
      let size = 8 * (n - on_stack) in
      Add (`rsp, Imm.of_int size) :: init

  (** [rev_pass_args ~init ~m ~n ts] is a sequence of instructions that
      passes the arguments stored in [ts] in reverse order assuming the
      function to be called has [m] return values and [n] args, followed
      by init *)
  let rev_pass_args ~init ~m ~n =
    let init, off =
      if m <= 2 then (init, 0) else (rev_alloc_ret ~init m, 1)
    in
    let f i s t = pass_args (`Temp t) (n - i + off) :: s in
    List.foldi ~init ~f

  (** [get_ret i] is an instruction that moves return register [i] into
      the virtual return register [`Rv i], or pops it from the stack *)
  let get_ret = function
    | 1 -> Mov (`Rv 1, `rax)
    | 2 -> Mov (`Rv 2, `rdx)
    | m -> Pop (`Rv m)

  (** [rev_get_rets ~init ~m] is a sequence of instructions that moves
      [m] return values into their virtual return registers, in reverse
      order, followed by [init] *)
  let rev_get_rets ~init ~m =
    let ret_indices =
      List.range ~start:`inclusive ~stop:`inclusive 1 m
    in
    let get acc i = get_ret i :: acc in
    List.fold ret_indices ~f:get ~init

  (** [rev_munch_call ~init ~gensym m e es] is the translation of
      lowered, reordered IR statement [`Call (m, e, es)], in reverse
      order, followed by [init]. *)
  let rev_munch_call ~init ~gensym m e es =
    let s1, name = Expr.rev_munch_label ~init ~gensym e in
    let s2, ts = Expr.rev_munch_list ~init:s1 ~gensym es in
    (* [n] is the number of args *)
    let n = List.length es in
    let call = Call name :: rev_pass_args ~init:s2 ~m ~n ts in
    let dealloc = rev_dealloc_args ~init:call ~n es in
    rev_get_rets ~init:(dealloc ~m) ~m

  (** [rev_munch_move ~init ~gensym e1 e2] is the translation of
      [`Move (e1, e2)], in reverse order, followed by [init] *)
  let rev_munch_move ~init ~gensym (e1 : Ir.Lir.dest) e2 : translation =
    (* TODO : eliminate uneeded temps *)
    match e1 with
    | `Mem addr ->
        let s1, loc = Expr.rev_munch_addr ~init ~gensym addr in
        let s2, e2 = Expr.rev_munch_ri ~init:s1 ~gensym e2 in
        Mov (`Mem loc, e2) :: s2
    | `Temp _ as t1 ->
        let s, e2 = Expr.rev_munch_rmi ~init ~gensym e2 in
        Mov (t1, e2) :: s

  (** [return_ith ~ret t i] moves [t] into into return register [i] or
      the appropriate memory address *)
  let return_ith ~ret t = function
    | 1 -> Mov (`rax, t)
    | 2 -> Mov (`rdx, t)
    | i ->
        let offset = Int64.of_int (8 * (i - 3)) in
        Mov (`Mem (Mem.create ~offset ret), t)

  (** [rev_munch_return ~init ~gensym ~ret es] is the translation of
      [`Return es], in reverse order, followed by [init] *)
  let rev_munch_return ~init ~gensym ~ret es : translation =
    let s, ts = Expr.rev_munch_list ~init ~gensym es in
    let f i s t = return_ith ~ret (`Temp t) (Int.succ i) :: s in
    Ret :: Leave :: List.foldi (List.rev ts) ~init:s ~f

  (** [rev_munch ~init ~gensym ~ret e] is the translation of [e], in
      reverse order, followed by [init] *)
  let rev_munch ~init ~gensym ~ret : Ir.Reorder.stmt -> translation =
    function
    | `Jump e -> rev_munch_jump ~init ~gensym e
    | `CJump (e, l) -> rev_munch_cjump ~init ~gensym e l
    | `Label l -> rev_munch_label ~init l
    | `Call (i, e, es) -> rev_munch_call ~init ~gensym i e es
    | `Move (e1, e2) -> rev_munch_move ~init ~gensym e1 e2
    | `Return es -> rev_munch_return ~init ~gensym ~ret es

  (** [munch ~gensym s] is the sequence of abstract assembly
      instructions having the same effect as [s] *)
  let munch ~gensym ~ret s =
    rev_munch ~init:[] ~gensym ~ret s |> List.rev

  (** [rev_munch_list stmts] is the translation of [stmts], in reverse
      order, followed by [init] *)
  let rev_munch_list ~gensym ~init ~ret :
      Ir.Reorder.stmt list -> translation =
    let f init = rev_munch ~gensym ~init ~ret in
    List.fold ~f ~init
end

(** [Toplevel] contains functions for manipulating IR toplevel
    statements and translating them into abstract assembly *)
module Toplevel = struct
  type translation = t list
  (** A [translation] is a list of abstract assembly instructions *)

  (** [pop_args ~m i] moves arg register [i] into virtual arg register
      [`Arg i] depending on the value of [m], or pops it from the stack *)
  let pop_args ~m i =
    let arg = `Arg i in
    match if m <= 2 then i else succ i with
    | 1 -> Mov (arg, `rdi)
    | 2 -> Mov (arg, `rsi)
    | 3 -> Mov (arg, `rdx)
    | 4 -> Mov (arg, `rcx)
    | 5 -> Mov (arg, `r8)
    | 6 -> Mov (arg, `r9)
    | j ->
        let offset = Int64.of_int (8 * (j - 5)) in
        Mov (arg, `Mem (Mem.create ~offset `rbp))

  (** [munch_fn ~gensym stmts ~n ~m] is the abstract assembly
      translation of the __body__ of [`Func (l, stmts, n, m)] *)
  let munch_fn ~gensym stmts ~n ~m : translation =
    let ret = `Temp (gensym ()) in
    let f i = pop_args ~m (succ i) in
    (* List.init starts at 0 *)
    let args = n |> List.init ~f |> List.rev in
    let f = if m <= 2 then Fn.id else List.cons (Mov (ret, `rdi)) in
    stmts
    |> Stmt.rev_munch_list ~gensym ~init:args ~ret
    |> List.rev |> f
end

let munch ~gensym top =
  let f = function
    | `Func (name, stmts, n, m) ->
        let body =
          Stmt.align :: Toplevel.munch_fn ~gensym stmts ~n ~m
        in
        First (Asm.Fn.create ~name ~body)
    | `Data (label, value) -> Second (Asm.Data.create ~label ~value)
  in
  let fns, data = List.partition_map top ~f in
  let globs = List.map ~f:Asm.Fn.name fns in
  Asm.Directive.
    [ IntelSyntax `noprefix; Data data; Globl globs; Text fns ]

module Asm = struct
  type t = Operand.Abstract.t Generic.Asm.t

  let to_string = Generic.Asm.to_string ~f:Operand.Abstract.to_string
end
