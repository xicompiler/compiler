type ('v, 'e) vertex = {
  value : 'v;
  incoming : ('v, 'e) edge list;
  outgoing : ('v, 'e) edge list;
}

and ('v, 'e) edge = {
  src : ('v, 'e) vertex;
  dst : ('v, 'e) vertex;
  weight : 'e;
}

module Vertex = struct
  type ('v, 'e) t = ('v, 'e) vertex

  let incoming { incoming } = incoming
  let outgoing { outgoing } = outgoing
  let value { value } = value
end

module Edge = struct
  type ('v, 'e) t = ('v, 'e) edge

  let src { src } = src
  let dst { dst } = dst
  let weight { weight } = weight
end
