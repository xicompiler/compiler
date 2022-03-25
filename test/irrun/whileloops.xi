use io
use conv

testWhile(a:int) : int {
  println(unparseInt(a))
  b:int = a
  while(b > 0) {
    b = b - 1
  }
  return b
}

main(args:int[][]) {
  a:int = 10

  b:int = testWhile(a)
  
  println(unparseInt(b))
}