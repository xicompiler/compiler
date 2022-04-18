open Core
open Generic

type t = Operand.Abstract.t Generic.t

(** [zero e] zeroes the contents of [e] *)
let zero e : t = Xor (e, (e :> Operand.Abstract.t))

module Expr = struct
  type translation = t list * string

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

  (** [rev_munch2 ~init ~gensym e1 e2] is [(s, t1, t2)], where [s] is
      the reverse sequence of instructions to move [e1] into [t1], then
      [e2] into [t2] *)
  and rev_munch2 ~init ~gensym e1 e2 =
    let s1, t1 = rev_munch ~init ~gensym e1 in
    let s2, t2 = rev_munch ~init:s1 ~gensym e2 in
    (s2, t1, t2)

  (** [rev_munch2_map ~f ~init:\[sn; ...; s1\] ~gensym e1 e2] is
      [\[f t1 t2; si; ...; sj; ...; sn; ... s1\]] where
      [\[sj; ...; sn+1\]] move the translation of [e1] into [t1] in
      reverse order and [si; ...; sj+1] move the translation of [e2]
      into [t2] in reverse order *)
  and rev_munch2_map ~f ~init ~gensym e1 e2 =
    let s, t1, t2 = rev_munch2 ~init ~gensym e1 e2 in
    (f t1 t2 :: s, t1)

  (** [rev_munch_mem ~init ~gensym e] is the translation of the lowered,
      reordered IR statement [`Mem e], followed by init *)
  and rev_munch_mem ~init ~gensym e : translation =
    (* TODO : better tiling *)
    let s, t = rev_munch ~init ~gensym e in
    let tmp = `Temp t in
    let mem = Mem.create tmp in
    (Mov (tmp, `Mem mem) :: s, t)

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
      lowered, reordered IR statement [`Bop (`And, e1, e2)], followed by
      init *)
  and rev_munch_add ~init ~gensym e1 e2 =
    let s, t1, t2 = rev_munch2 ~init ~gensym e1 e2 in
    let index = Mem.Index.create (`Temp t2) in
    let sum = Mem.create (`Temp t1) ~index in
    let t3 = gensym () in
    (Lea (`Temp t3, `Mem sum) :: s, t3)

  (** [rev_munch_and ~init ~gensym e1 e2] is the translation of the
      lowered, reordered IR statement [`Bop (`And, e1, e2)], followed by
      init *)
  and rev_munch_and ~init =
    let f t1 t2 = And (`Temp t1, `Temp t2) in
    rev_munch2_map ~f ~init

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
    let f t1 t2 = IMul (`RM (`Temp t1, `Temp t2)) in
    rev_munch2_map ~f ~init

  (** [rev_munch_or ~init ~gensym e1 e2] is the translation of the
      lowered, reordered IR statement [`Bop (`Or, e1, e2)], followed by
      init *)
  and rev_munch_or ~init =
    let f t1 t2 = Or (`Temp t1, `Temp t2) in
    rev_munch2_map ~f ~init

  (** [rev_munch_sub ~init ~gensym e1 e2] is the translation of the
      lowered, reordered IR statement [`Bop (`Sub, e1, e2)], followed by
      init *)
  and rev_munch_sub ~init =
    let f t1 t2 = Sub (`Temp t1, `Temp t2) in
    rev_munch2_map ~f ~init

  (** [rev_munch_xor ~init ~gensym e1 e2] is the translation of the
      lowered, reordered IR statement [`Bop (`Xors, e1, e2)], followed
      by init *)
  and rev_munch_xor ~init =
    let f t1 t2 = Xor (`Temp t1, `Temp t2) in
    rev_munch2_map ~f ~init

  (** [rev_munch_cmp ~init ~gensym e1 e2] is the translation of the
      lowered, reordered IR statement [`Bop (cmp, e1, e2)], where [cmp]
      is a comparison operator, followed by init *)
  and rev_munch_cmp ~init ~gensym op e1 e2 =
    let s, t1, t2 = rev_munch2 ~init ~gensym e1 e2 in
    let t3 = gensym () in
    let cc = ConditionCode.of_cmp op in
    (Setcc (cc, `Temp t3) :: Cmp (`Temp t1, `Temp t2) :: s, t3)

  (** [rev_munch_list ~init:\[si; ...; sj\] ~gensym \[e1; ...; en\]] is
      a pair [(\[sj; ...; si; ...; s1\], \[tn; ...; t1\])] where
      [\[s1; ...; si-1\]] effects the moves of the translation of each
      [ei] into [ti] *)
  let rev_munch_list ~init ~gensym es =
    let f (init, ts) e =
      let s, t = rev_munch ~init ~gensym e in
      (s, t :: ts)
    in
    es |> List.fold ~init:(init, []) ~f

  (** [rev_munch_label ~init ~gensym e] is [(init, `Name l)] if [e] is
      [`Name l] and [(s, `Temp t)] where [(s, t)] is
      [rev_munch ~init ~gensym e] otherwise *)
  let rev_munch_label ~init ~gensym = function
    | `Name _ as name -> (init, name)
    | e ->
        let s, t = rev_munch ~init ~gensym e in
        (s, `Temp t)

  let munch ~gensym e =
    e |> rev_munch ~init:[] ~gensym |> Tuple2.map_fst ~f:List.rev
end

module Stmt = struct
  type translation = t list

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

  (** [rev_dealloc_args ~init es] is a sequence of instructions that
      frees any stack space used to store the translations of arguments
      [es], in reverse order, followed by [init] *)
  let rev_dealloc_args ~init es : translation =
    (* number of args *)
    let n = List.length es in
    (* if <= 6 args, none were pushed to stack *)
    if n <= 6 then init
    else
      (* stack space occupied by args *)
      let size = 8 * (n - 6) in
      Add (`rsp, Imm.of_int size) :: init

  (** [rev_pass_args ~init ~m ts] is a sequence of instructions that
      passes the arguments stored in [ts] in reverse order assuming the
      function to be called has [m] return values , followed by init.*)
  let rev_pass_args ~init ~m ts =
    let init, off =
      if m <= 2 then (init, 1) else (rev_alloc_ret ~init m, 2)
    in
    let f i s t = pass_args (`Temp t) (i + off) :: s in
    List.foldi ts ~init ~f

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
      List.range ~stride:~-1 ~start:`inclusive ~stop:`inclusive m 1
    in
    let get acc i = get_ret i :: acc in
    List.fold ret_indices ~f:get ~init

  (** [rev_munch_call ~init ~gensym m e es] is the translation of
      lowered, reordered IR statement [`Call (m, e, es)], in reverse
      order, followed by [init]. *)
  let rev_munch_call ~init ~gensym m e es =
    let s1, name = Expr.rev_munch_label ~init ~gensym e in
    let s, ts = Expr.rev_munch_list ~init:s1 ~gensym es in
    let call = Call name :: align :: rev_pass_args ~init ~m ts in
    let dealloc = rev_dealloc_args ~init:call es in
    rev_get_rets ~init:dealloc ~m

  (** [rev_munch_move ~init ~gensym e1 e2] is the translation of
      [`Move (e1, e2)], in reverse order, followed by [init] *)
  let rev_munch_move ~init ~gensym (e1 : Ir.Lir.dest) e2 : translation =
    (* TODO : eliminate uneeded temps *)
    match e1 with
    | `Mem addr ->
        let s, addr, t2 = Expr.rev_munch2 ~init ~gensym addr e2 in
        let mem = Mem.create (`Temp addr) in
        Mov (`Mem mem, `Temp t2) :: s
    | `Temp _ as t1 ->
        let s, t2 = Expr.rev_munch ~init ~gensym e2 in
        Mov (t1, `Temp t2) :: s

  (** [push_rets t i] moves [t] into into return register [i] or pushes
      it to the stack *)
  let push_rets t = function
    | 1 -> Mov (`rax, t)
    | 2 -> Mov (`rdx, t)
    | _ -> Push t

  (** [rev_munch_return ~init ~gensym es] is the translation of
      [`Return es], in reverse order, followed by [init] *)
  let rev_munch_return ~init ~gensym es : translation =
    let s, ts = Expr.rev_munch_list ~init ~gensym es in
    let f i s t = push_rets (`Temp t) (Int.succ i) :: s in
    Ret :: Leave :: List.foldi ts ~init:s ~f

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

  let munch ~gensym s = rev_munch ~init:[] ~gensym s |> List.rev

  (** [rev_munch_list stmts] is the translation of [stmts], in reverse
      order, followed by [init] *)
  let rev_munch_list ~gensym ~init : Ir.Reorder.stmt list -> translation
      =
    let f init = rev_munch ~init ~gensym in
    List.fold ~f ~init
end

module Toplevel = struct
  type translation = t list

  (** [pop_args ~n_rets i] moves arg register [i] into virtual arg
      register [`Arg i] depending on the value of [n_rets], or pops it
      from the stack *)
  let pop_args ~n_rets i =
    let arg = `Arg i in
    match if n_rets <= 2 then i else succ i with
    | 1 -> Mov (arg, `rdi)
    | 2 -> Mov (arg, `rsi)
    | 3 -> Mov (arg, `rdx)
    | 4 -> Mov (arg, `rcx)
    | 5 -> Mov (arg, `r8)
    | 6 -> Mov (arg, `r9)
    | _ -> Pop arg

  (** [munch_fn l stmts n_args n_rets] is the abstract assembly
      translation of the __body__ of [`Func (l, stmts, n_args, n_rets)] *)
  let munch_fn ~gensym stmts ~n_args ~n_rets : translation =
    let f i = pop_args ~n_rets (Int.succ i) in
    (* List.init starts at 0 *)
    let args = n_args |> List.init ~f |> List.rev in
    stmts |> Stmt.rev_munch_list ~gensym ~init:args |> List.rev
end

let munch ~gensym top =
  let f = function
    | `Func (name, stmts, n_args, n_rets) ->
        let body = Toplevel.munch_fn ~gensym stmts ~n_args ~n_rets in
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
