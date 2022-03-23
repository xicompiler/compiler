(** [Mir] is a mid level intermediate representation *)
module Mir : module type of struct
  include Mir
end

(** [Lir] is a low level intermediate representation *)
module Lir : module type of struct
  include Lir
end

(** [Reorder] contains functions for reordering lowered IR *)
module Reorder : module type of struct
  include Reorder
end
