open Core
open Option.Let_syntax
open Generic
open Util.Fn

type t = Operand.Abstract.t Generic.t [@@deriving sexp]

(** [map_imul] applies [f] to every operand within [enc] based on [map] *)
let map_imul enc ~map ~f =
  match enc with
  | `M e -> `M (map e ~f)
  | `RM (e1, e2) -> `RM (map e1 ~f, map e2 ~f)
  | `RMI (e1, e2, i) -> `RMI (map e1 ~f, map e2 ~f, i)

(** [map_instr instr ~map ~f] applies concretizing function [f] to every
    operand within [instr] based on [map] *)
let map_instr instr ~map ~f =
  match instr with
  | (Label _ | Enter _ | Jcc _ | Leave | Ret _) as instr -> instr
  | Jmp e -> Jmp (map e ~f)
  | Setcc (cc, e) -> Setcc (cc, map e ~f)
  | Cmp (e1, e2) -> Cmp (map e1 ~f, map e2 ~f)
  | Test (e1, e2) -> Test (map e1 ~f, map e2 ~f)
  | Push e -> Push (map e ~f)
  | Pop e -> Pop (map e ~f)
  | IMul enc -> IMul (map_imul enc ~map ~f)
  | Inc e -> Inc (map e ~f)
  | Dec e -> Dec (map e ~f)
  | Call call -> Call { call with name = map call.name ~f }
  | IDiv e -> IDiv (map e ~f)
  | Shl (e, i) -> Shl (map e ~f, i)
  | Shr (e, i) -> Shr (map e ~f, i)
  | Sar (e, i) -> Sar (map e ~f, i)
  | Add (e1, e2) -> Add (map e1 ~f, map e2 ~f)
  | Sub (e1, e2) -> Sub (map e1 ~f, map e2 ~f)
  | Xor (e1, e2) -> Xor (map e1 ~f, map e2 ~f)
  | And (e1, e2) -> And (map e1 ~f, map e2 ~f)
  | Or (e1, e2) -> Or (map e1 ~f, map e2 ~f)
  | Lea (e1, e2) -> Lea (map e1 ~f, map e2 ~f)
  | Mov (e1, e2) -> Mov (map e1 ~f, map e2 ~f)
  | Movzx (e1, e2) -> Movzx (map e1 ~f, map e2 ~f)

let map = map_instr ~map:Operand.Abstract.map
let map_list ~f = List.map ~f:(map_instr ~map:Operand.Abstract.map ~f)

let map_concrete_list ~f =
  List.map ~f:(map_instr ~map:Operand.Abstract.map_concrete ~f)

(** [Expr] contains functions for manipulating IR expressions and
    translating them into abstract assembly *)
module Expr = struct
  type translation = t list * string
  (** A [translation] is a pair [(s, t)] where [s] is a sequence of
      abstract assembly instructions necessary to effect the movement of
      the translation of an expression into temporary [t] *)

  (** [rev_munch_name ~init ~gensym l] is the translation of the
      lowered, reordered IR statement [`Name l], followed by init *)
  let rev_munch_name ~init ~gensym l : translation =
    let t = gensym () in
    let mem = Mem.create ~segment:l `rip in
    (Lea (`Temp t, `Mem mem) :: init, t)

  (** [rev_munch_const ~init ~gensym i] is the translation of the
      lowered, reordered IR statement [`Const i], followed by init *)
  let rev_munch_const ~init ~gensym i : translation =
    let t = gensym () in
    (Mov (`Temp t, `Imm i) :: init, t)

  (** [rev_munch_virtual ~init ~gensym v] is the translation of moving a
      virtual arg or register into a fresh temp, followed by init *)
  let rev_munch_virtual ~init ~gensym v : translation =
    let t = gensym () in
    (Mov (`Temp t, v) :: init, t)

  let rec rev_munch ~init ~gensym : Ir.Lir.expr -> translation =
    function
    | `Name l -> rev_munch_name ~init ~gensym l
    | `Const i -> rev_munch_const ~init ~gensym i
    | `Bop (op, e1, e2) -> rev_munch_bop ~init ~gensym op e1 e2
    | `Temp t -> (init, t)
    | `Mem e -> rev_munch_mem ~init ~gensym e
    | (`Arg _ | `Rv _) as t -> rev_munch_virtual ~init ~gensym t

  and rev_munch_index ?offset ?scale ~init ~gensym ~index base =
    let s, base, index = rev_munch2 ~init ~gensym base index in
    let index = Mem.Index.create ?scale (`Temp index) in
    (s, Mem.create ~index ?offset (`Temp base))

  and rev_munch_base ~init ~gensym ?offset base =
    let s, base = rev_munch ~init ~gensym base in
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
  and rev_munch2 ~init ~gensym e1 e2 =
    let s1, t1 = rev_munch ~init ~gensym e1 in
    let s2, t2 = rev_munch ~init:s1 ~gensym e2 in
    (s2, t1, t2)

  and rev_munch_operand ~init ~gensym = function
    | `Mem addr ->
        let s, mem = rev_munch_addr ~init ~gensym addr in
        (s, `Mem mem)
    | e ->
        let s, t = rev_munch ~init ~gensym e in
        (s, `Temp t)

  and rev_munch_rmi ~init ~gensym = function
    | `Const i -> (init, `Imm i)
    | e -> rev_munch_operand ~init ~gensym e

  and rev_munch_ri ~init ~gensym = function
    | `Const i -> (init, `Imm i)
    | e ->
        let s, t = rev_munch ~init ~gensym e in
        (s, `Temp t)

  (** [rev_munch2_map ~f ~init:\[sn; ...; s1\] ~gensym e1 e2] is
      [\[f t1 t2; si; ...; sj; ...; sn; ... s1\]] where
      [\[sj; ...; sn+1\]] move the translation of [e1] into [t1] in
      reverse order and [si; ...; sj+1] move the translation of [e2]
      into [t2] in reverse order *)
  and rev_munch2_map ~f ~init ~gensym e1 e2 =
    let s1, e1 = rev_munch_operand ~init ~gensym e1 in
    let s2, e2 = rev_munch_operand ~init:s1 ~gensym e2 in
    let dst = gensym () in
    let t = `Temp dst in
    (f t e2 :: Mov (`Temp dst, e1) :: s2, dst)

  and rev_munch_addr ~init ~gensym = function
    | `Bop (`Add, e1, e2) -> rev_munch_mem_sum ~init ~gensym e1 e2
    | e ->
        let s, t = rev_munch ~init ~gensym e in
        (s, Mem.create (`Temp t))

  (** [rev_munch_mem ~init ~gensym e] is the translation of the lowered,
      reordered IR statement [`Mem e], followed by init *)
  and rev_munch_mem ~init ~gensym e : translation =
    (* TODO : better tiling *)
    let s, mem = rev_munch_addr ~init ~gensym e in
    let t = gensym () in
    (Mov (`Temp t, `Mem mem) :: s, t)

  (** [rev_munch_bop ~init ~gensym op e1 e2] is the translation of the
      lowered, reordered IR statement [`Bop (op, e1, e2)], followed by
      init *)
  and rev_munch_bop ~init ~gensym op =
    match op with
    | `Add -> rev_munch_add ~init ~gensym
    | `And -> rev_munch_and ~init ~gensym
    | `Div -> rev_munch_div ~init ~gensym
    | `HMul -> rev_munch_hmul ~init ~gensym
    | `Mod -> rev_munch_mod ~init ~gensym
    | `Mul -> rev_munch_mul ~init ~gensym
    | `Or -> rev_munch_or ~init ~gensym
    | `Sub -> rev_munch_sub ~init ~gensym
    | `Xor -> rev_munch_xor ~init ~gensym
    | #Ir.Op.cmp as op -> rev_munch_cmp ~init ~gensym op

  (** [rev_munch_add ~init ~gensym e1 e2] is the translation of the
      lowered, reordered IR statement [`Bop (`Add, e1, e2)], followed by
      init *)
  and rev_munch_add ~init ~gensym e1 e2 =
    let s, sum = rev_munch_mem_sum ~init ~gensym e1 e2 in
    let t3 = gensym () in
    (Lea (`Temp t3, `Mem sum) :: s, t3)

  (** [rev_munch_and ~init ~gensym e1 e2] is the translation of the
      lowered, reordered IR statement [`Bop (`And, e1, e2)], followed by
      init *)
  and rev_munch_and ~init = rev_munch2_map ~f:and_ ~init

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
    let call = Call { name; n; m } :: rev_pass_args ~init:s2 ~m ~n ts in
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
    Ret (List.length ts) :: Leave :: List.foldi (List.rev ts) ~init:s ~f

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

(** [arg_regs] is the argument registers used in a call instruction *)
let arg_regs = [ `rdi; `rsi; `rdx; `rcx; `r8; `r9 ]

(** [ret_regs] is the return registers used in a call instruction *)
let ret_regs = [ `rax; `rdx ]

(** [regs_of_ops ?init ops] is the set of immediate abstract regs in
    [ops] *)
let rec regs_of_ops ?(init = Reg.Abstract.Set.empty) = function
  | (#Reg.Abstract.t as h) :: t -> regs_of_ops ~init:(Set.add init h) t
  | _ -> init

(** [def_of_imul m] is [Some op] for the destination operand in an
    [imul] instruction *)
let def_of_imul = function
  | `M _ -> regs_of_ops [ `rax; `rdx ]
  | `RM (op, _) | `RMI (op, _, _) -> regs_of_ops [ op ]

(** [def_of_idiv] is the defined variables in an [idiv] instruction *)
let def_of_idiv = regs_of_ops [ `rax; `rdx ]

(** [def_of_call c] is the defined variables in a [call] instruction *)
let def_of_call { m } =
  Reg.Abstract.Set.of_list (Util.List.first_n ret_regs m)

let def : t -> Reg.Abstract.Set.t = function
  | Label _ | Enter _ | Jmp _ | Jcc _ | Cmp _ | Test _ | Push _ | Leave
  | Ret _ ->
      Reg.Abstract.Set.empty
  | Setcc (_, op)
  | Shl (op, _)
  | Shr (op, _)
  | Sar (op, _)
  | Add (op, _)
  | Sub (op, _)
  | Xor (op, _)
  | And (op, _)
  | Or (op, _)
  | Lea (op, _)
  | Mov (op, _)
  | Movzx (op, _)
  | Pop op
  | Inc op
  | Dec op ->
      regs_of_ops [ op ]
  | IDiv _ -> def_of_idiv
  | IMul m -> def_of_imul m
  | Call c -> def_of_call c

(** [regs_of_ops_deep ?init ops] is the set of abstract registers in
    [ops], including within memory addresses *)
let rec regs_of_ops_deep ?(init = Reg.Abstract.Set.empty) = function
  | `Mem m :: t ->
      let mem = regs_of_ops (Mem.ops m) in
      regs_of_ops_deep ~init:(Set.union init mem) t
  | h :: t ->
      let init = Set.union init (regs_of_ops [ h ]) in
      regs_of_ops_deep ~init t
  | [] -> init

(** [use_of_idiv op] is the used variables in an [idiv] instruction *)
let use_of_idiv op = regs_of_ops_deep [ op; `rax; `rdx ]

(** [use_of_imul m] is the defined variables in an [imul] instruction *)
let use_of_imul = function
  | `M op -> regs_of_ops_deep [ op; `rax ]
  | `RM (op1, op2) -> regs_of_ops_deep [ op1; op2 ]
  | `RMI (_, op, _) -> regs_of_ops_deep [ op ]

(* [use_of_call c] is the used variables in a [call] instruction *)
let use_of_call { n; m } =
  Reg.Abstract.Set.of_list
    (Util.List.first_n arg_regs (if m <= 2 then n else succ n))

(* [use_of_ret m] is the used variables in a [ret] instruction with [m]
   number of returns *)
let use_of_ret m =
  Reg.Abstract.Set.of_list (Util.List.first_n ret_regs m)

(** [use_of_mov op1 op2] is the used variables in a [mov] instruction *)
let use_of_mov op1 op2 =
  let init = regs_of_ops_deep [ op2 ] in
  match op1 with
  | `Mem m ->
      let mem = regs_of_ops (Mem.ops m) in
      Set.union init mem
  | _ -> init

let use : t -> Reg.Abstract.Set.t = function
  | Label _ | Enter _ | Jcc _ | Setcc _ | Pop _ | Leave ->
      Reg.Abstract.Set.empty
  | Jmp op
  | Shl (op, _)
  | Shr (op, _)
  | Sar (op, _)
  | Push op
  | Inc op
  | Dec op ->
      regs_of_ops_deep [ op ]
  | Add (op1, op2)
  | Sub (op1, op2)
  | Xor (op1, op2)
  | And (op1, op2)
  | Or (op1, op2)
  | Cmp (op1, op2)
  | Test (op1, op2)
  | Lea (op1, op2)
  | Movzx (op1, op2) ->
      regs_of_ops_deep [ op1; op2 ]
  | IDiv d -> use_of_idiv d
  | IMul m -> use_of_imul m
  | Call c -> use_of_call c
  | Ret m -> use_of_ret m
  | Mov (op1, op2) -> use_of_mov op1 op2

(** [regs_of_imul m] is the set of registers in an [imul] instruction *)
let regs_of_imul = function
  | `M op -> regs_of_ops_deep [ op ]
  | `RM (op1, op2) | `RMI (op1, op2, _) -> regs_of_ops_deep [ op1; op2 ]

let regs : t -> Reg.Abstract.Set.t = function
  | Label _ | Enter _ | Jcc _ | Leave | Ret _ -> Reg.Abstract.Set.empty
  | Jmp e
  | Setcc (_, e)
  | Push e
  | Pop e
  | Inc e
  | Dec e
  | Call { name = e }
  | IDiv e
  | Shl (e, _)
  | Shr (e, _)
  | Sar (e, _) ->
      regs_of_ops_deep [ e ]
  | Cmp (e1, e2)
  | Test (e1, e2)
  | Add (e1, e2)
  | Sub (e1, e2)
  | Xor (e1, e2)
  | And (e1, e2)
  | Or (e1, e2)
  | Lea (e1, e2)
  | Mov (e1, e2)
  | Movzx (e1, e2) ->
      regs_of_ops_deep [ e1; e2 ]
  | IMul enc -> regs_of_imul enc
