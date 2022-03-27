open OUnit2
open Ir
open Mir

(** [pos] is a dummy position *)
let pos =
  let open Position in
  { line = 0; column = 0 }

(** [id_with_pos id] is [id] with dummy position information *)
let id_with_pos = Node.Position.make ~pos

(** [mangle_test ~name ~expect ~ctx id] constructs an OUnit test with
    name [name] asserting that [mangle id ~ctx] is equal to [expect] *)
let mangle_test ~name ~expect ~ctx id =
  name >:: fun _ ->
  let actual = mangle (id_with_pos id) ~ctx in
  assert_equal expect actual

(** [ctx_with_fns ~ctx fns] is the context [ctx] with [fns], where a
    single element in [fns] is a tuple [(name,arg,ret)] for the function
    with name [name], argument type [arg], and return type [ret] *)
let rec ctx_with_fns_acc ~ctx = function
  | (id, arg, ret) :: t -> (
      match Context.add_fn_decl ~id:(id_with_pos id) ~arg ~ret ctx with
      | Ok ctx -> ctx_with_fns_acc ~ctx t
      | Error e -> failwith "fn id bound in ctx")
  | [] -> ctx

(** [ctx_with_fns fns] is the empty context with [fns] added *)
let ctx_with_fns = ctx_with_fns_acc ~ctx:Context.empty

(* main (int[][]) : unit
 * unparseInt (int) : int[]
 * parseInt (int[]) : int, bool
 * eof () bool
 * gcd (int, int) : int
 * multiple__underScores () : unit
 *)
let examples_ctx =
  ctx_with_fns
    [
      ("main", `Array (`Array `Int), `Unit);
      ("unparseInt", `Int, `Array `Int);
      ("parseInt", `Array `Int, `Tuple [ `Int; `Bool ]);
      ("eof", `Tuple [], `Bool);
      ("gcd", `Tuple [ `Int; `Int ], `Int);
      ("multiple__underScores", `Tuple [], `Unit);
    ]

let suite =
  "mangle test suite"
  >::: [
         mangle_test ~name:"main" ~expect:"_Imain_paai"
           ~ctx:examples_ctx "main";
         mangle_test ~name:"unparseInt" ~expect:"_IunparseInt_aii"
           ~ctx:examples_ctx "unparseInt";
         mangle_test ~name:"parseInt" ~expect:"_IparseInt_t2ibai"
           ~ctx:examples_ctx "parseInt";
         mangle_test ~name:"eof" ~expect:"_Ieof_b" ~ctx:examples_ctx
           "eof";
         mangle_test ~name:"gcd" ~expect:"_Igcd_iii" ~ctx:examples_ctx
           "gcd";
         mangle_test ~name:"multiple__underScores"
           ~expect:"_Imultiple____underScores_p" ~ctx:examples_ctx
           "multiple__underScores";
       ]
