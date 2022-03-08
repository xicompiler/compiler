use intf;

a(): int {
  return 1;
}
b(x:int, y:int) {
  w:int = x;
  z:int = y;
}
c(w:int[][], x:int, y:int, z:bool):bool {
  return w[x][y] == 1 | z;
}