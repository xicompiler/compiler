a(): int {
  return 1;
}
b(x:int, y:int) {
  a:int = x;
  b:int = y;
}
c(w:int[][], x:int, y:int, z:bool):bool {
  return w[x][y] == 1 | z;
}