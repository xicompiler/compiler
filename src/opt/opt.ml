type phases = {
  initial : bool;
  final : bool;
}

type t = {
  optir : phases;
  optcfg : phases;
  cf : bool;
  reg : bool;
  copy : bool;
  dce : bool;
  cp : bool;
  vn : bool;
}

let enabled opt =
  { opt with cf = true; reg = true; copy = true; dce = true; cp = true }

let disabled =
  {
    optir = { initial = false; final = false };
    optcfg = { initial = false; final = false };
    cf = false;
    reg = false;
    copy = false;
    dce = false;
    cp = false;
    vn = false;
  }
