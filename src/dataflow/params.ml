open! Core

type 'data meet = 'data list -> 'data

type direction =
  [ `Forward
  | `Backward
  ]

type ('data, 'v) t = {
  f : data:'data -> vertex:'v -> 'data;
  meet : 'data meet;
  top : 'data;
  direction : direction;
  equal : 'data -> 'data -> bool;
}
