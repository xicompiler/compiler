(** [col_offset] is the offset between the lexer buffer "column" and the
    true file column *)
let col_offset = 1

type t = {
  line : int;
  column : int;
}

let format { line; column } = Printf.sprintf "%d:%d %s" line column

type position = t

module Error = struct
  type 'a t = {
    cause : 'a;
    position : position;
  }

  let create ~pos cause = { cause; position = pos }
  let cause { cause } = cause
  let position { position } = position
  let format { cause; position } ~f = cause |> f |> format position

  let format_last e ~f ~fmt ~msg =
    e |> format ~f |> Printf.sprintf fmt msg
end

type 'a error = 'a Error.t

module Entry = struct
  type 'a t = ('a, position) Entry.t

  let error ~cause e =
    let pos = Entry.data e in
    Error.create ~pos cause
end

type 'a entry = 'a Entry.t

let of_lexer (p : Lexing.position) =
  { line = p.pos_lnum; column = col_offset + p.pos_cnum - p.pos_bol }

let of_lexbuf Lexing.{ lex_start_p; _ } = of_lexer lex_start_p
