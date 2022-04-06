open Core

type ('a, 'b) t = 'a * 'b

let create ~key ~data = (key, data)
let key = fst
let data = snd

module Key = struct
  let fold (key, _) ~f = f key
  let map = Tuple2.map_fst
end

module Data = struct
  let fold (_, data) ~f = f data
  let set (key, _) ~data = create ~key ~data
end
