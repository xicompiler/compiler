f(x:int[], y:int) {
  x[0] = 0
  x[y] = 0
  x[{1,2,3}[0]] = 0
  x[g()] = 0
  x[x[0]] = 0
  x[-1] = 0
  x[0+0] = 0
  x[(0)] = 0
}

g():int {
  return 0
}
booleanboi :bool;