open! Core

type 'data t = {
  input : 'data;
  output : 'data;
}
[@@deriving fields]
