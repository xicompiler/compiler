main(args: int[][]) {
  a:int
  a, b: bool = 1, true

  // multiple uops
  c: int = ----1
  b = !!!!b;

  // trailing comma
  arr: int[] = { 1,2,3,4,5, }
  d: int[][] = {{1, 0,}, {0, 1,},}

  // negative ints
  e: int = -1 - -1 + -2 - -3;

  // T[][][] declarations
  g:int[][][] = {{{1}}}
  h:int[][][] = {{{1, 3}, {1, 2}}}
}

