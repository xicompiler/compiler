include module type of struct
  include Abstract
end

include module type of struct
  include Factory
end

(** [Position] is the type of a node with position information *)
module Position : module type of struct
  include Pos
end
