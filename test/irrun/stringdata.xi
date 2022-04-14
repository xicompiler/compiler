use io
use conv

main(args:int[][]) {
  i:int = 0
  while (i < 2) {
      a : int[] = "hello"
      if (i == 0) {
          a[0] = 'c'
      }
      println(a)
      i = i + 1
  }
}