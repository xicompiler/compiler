use io
use conv

global: int[]

main(args:int[][]) {
  global = {0, 0, 0}
  global[1] = 2;
  global[0] = 3;
  global[2] = global[1];
  println(unparseInt(global[2]))
}