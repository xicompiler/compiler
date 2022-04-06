open Core

type id = string Position.entry
(** An [id] is the type of a Xi identifier *)

type decl = id * Type.tau
(** A [decl] is the type of a Xi declaration represented as a pair
    [(id, t)] where [id] is the name of the identifier and [t] is its
    type. *)

(** An [Expr] represents an expression in the Xi langauge *)
module Expr : sig
  open Op

  type primitive = Primitive.t
  (** [primitive] is the type of a primitive value *)

  (** [t] is the type of an expression in the AST *)
  type 'e t =
    | Primitive of primitive
    | Id of id
    | Array of 'e nodes
    | String of string
    | Bop of binop * 'e node * 'e node
    | Uop of unop * 'e node
    | FnCall of 'e call
    | Length of 'e node
    | Index of 'e node * 'e node

  and 'e node = ('e t, 'e) Entry.t
  (** An ['e node] wraps an ['e t] and data of type ['e] *)

  and 'e nodes = 'e node list
  (** [nodes] is the type of a list of expression nodes *)

  and 'e call = id * 'e nodes
  (** A [call] is the type of a function call represented as a pair
      [(id, args)] where [id] is the name of the function and [args] is
      the list of arguments *)

  type ('a, 'e, 'acc) folder =
    primitive:(primitive -> 'e -> 'acc) ->
    id:(id -> 'e -> 'acc) ->
    array:('acc list -> 'e -> 'acc) ->
    string:(string -> 'e -> 'acc) ->
    bop:(binop -> 'acc -> 'acc -> 'e -> 'acc) ->
    uop:(unop -> 'acc -> 'e -> 'acc) ->
    fn_call:(id -> 'acc list -> 'e -> 'acc) ->
    length:('acc -> 'e -> 'acc) ->
    index:('acc -> 'acc -> 'e -> 'acc) ->
    'a
  (** An [('a, 'e, 'acc) folder] represents a fold over ['e t],
      accumulating a term of type ['acc], and producing a term of type
      ['a] *)
end

module Stmt : sig
  (** A [typ] is a Xi type whose arrays are optionally initialized with
      an expression of type [expr] *)

  (** A [stmt] is a legal statement in Xi, also known as a command. *)
  type ('e, 's) t =
    | If of 'e Expr.node * ('e, 's) node
    | IfElse of 'e Expr.node * ('e, 's) node * ('e, 's) node
    | While of 'e Expr.node * ('e, 's) node
    | VarDecl of decl
    | ArrayDecl of id * Type.tau * 'e Expr.node option list
    | Assign of id * 'e Expr.node
    | ArrAssign of 'e Expr.node * 'e Expr.node * 'e Expr.node
    | ExprStmt of 'e Expr.call
    | VarInit of id * Type.tau * 'e Expr.node
    | MultiAssign of decl option list * id * 'e Expr.nodes
    | PrCall of 'e Expr.call
    | Return of 'e Expr.nodes
    | Block of ('e, 's) block

  and ('e, 's) node = (('e, 's) t, 's) Entry.t
  (** [node] is the type of a statement node *)

  and ('e, 's) block = ('e, 's) node list
  (** A [block] is the type of a possible empty block of statements in
      Xi, represented as a list of statements possibly followed by a
      return statement *)
end

(** [Toplevel] represents the toplevel definitions of the AST *)
module Toplevel : sig
  (** [Sig] represents a function signature in Xi *)
  module Sig : sig
    type t
    (** A [t] is a signature or intf for an individual method where
        [types] is the list of (possibly none) return types. *)

    val create : name:id -> params:decl list -> ret:Type.tau list -> t
    (** [create ~name ~params ~types] is a signature with name [name],
        params [params] and return types [ret] *)

    val name : t -> id
    (** [name sg] is the name of signature [sg] *)

    val params : t -> decl list
    (** [params sg] is the list of parameters of signature [sg] *)

    val ret : t -> Type.tau list
    (** [ret sg] is the list of return types of signature [sg] *)
  end

  type ('e, 's) fn = Sig.t * ('e, 's) Stmt.block
  (** A [fn] is a Xi function definition whose body is a block of
      statements represented as a pair [(signature, body)] where
      [signature] is the function's signature and [body] is its body. *)

  (** A [definition] is the type of a top-level declaration in Xi:
      either a function definition, or declaration or initialization of
      a global variable. *)
  type ('e, 's) definition =
    | FnDefn of ('e, 's) fn
    | GlobalDecl of decl
    | GlobalInit of id * Type.tau * Expr.primitive

  type ('e, 's, 't) node = (('e, 's) definition, 't) Entry.t
  (** An [('e, 's, 't) node] wraps a toplevel node with expressions
      carrying data of type ['e], statements carrying data of type ['s],
      and toplevel definitions carrying type ['t] *)

  (** [Source] represents a source file in Xi *)
  module Source : sig
    type ('e, 's, 't) t
    (** A [t] describes the structure of a source file in Xi; 0 or more
        use statements followed by 1 or more top-level definitions, at
        least one of which must be a function definition. *)

    type 't use = (id, 't) Entry.t
    (** A ['t use] is an abstract [use] statement decorated with data of
        type ['t] *)

    val create :
      uses:'t use list -> defs:('e, 's, 't) node list -> ('e, 's, 't) t
    (** [create ~uses ~defs] is a source file with list of use
        statements [uses] and definitions [defs] *)

    val uses : ('e, 's, 't) t -> 't use list
    (** [uses src] is the list of use statements present in [src] *)

    val defs : ('e, 's, 't) t -> ('e, 's, 't) node list
    (** [defs src] is the list of definitions present in [src] *)
  end

  type 't intf = (Sig.t, 't) Entry.t list
  (** An [intf] is a Xi intf, represented as a non-empty list of
      function signatures. *)
end

(** An expression of type [t] is an expression representing a node of
    the Abstract Syntax Tree of a Xi program, described either by a
    source or intf file. *)
type ('e, 's, 't) t =
  | Source of ('e, 's, 't) Toplevel.Source.t
  | Intf of 't Toplevel.intf
[@@deriving variants]

val sexp_of_t : ('e, 's, 't) t -> Sexp.t
(** [sexp_of_t ast] is the s-expression serialization of [ast]. *)

val const_fold : ('e, 's, 't) t -> ('e, 's, 't) t
(** [const_fold ast] is [ast] where all constants expressions have been
    fully evaluated. Conditionals are also folded, if possible. *)

val iter_source :
  ('e, 's, 't) t -> f:(('e, 's, 't) Toplevel.Source.t -> unit) -> unit
(** [iter_source ast ~f] is [f src] if [ast] is [Source ast] and [()]
    otherwise *)
