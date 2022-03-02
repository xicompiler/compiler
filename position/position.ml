(** [col_offset] is the offset between the lexer buffer "column" and the
    true file column *)
let col_offset = 1

type t = {
  line : int;
  column : int;
}

let get_position (p : Lexing.position) =
  { 
    line = p.pos_lnum; 
    column = col_offset + p.pos_cnum - p.pos_bol 
  }

let get_position_lb ({ lex_start_p = p; _ } : Lexing.lexbuf) =
  get_position p
