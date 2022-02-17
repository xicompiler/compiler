open Core

let close cin cout =
  try
    In_channel.close cin;
    Out_channel.close cout
  with
  | e ->
      In_channel.close cin;
      Out_channel.close_no_err cout;
      raise e