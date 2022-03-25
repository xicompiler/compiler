use io
use conv

testBooleans(a:int) {
  b:int = 5
  if (a < b) println("correct")
  else println ("wrong")

  if (-b == -5) println("correct")
  else println ("wrong")

  if (b != -5) println("correct")
  else println ("wrong")

  if (b >= -5) println("correct")
  else println ("wrong")

  if (a <= 1) println("correct")
  else println ("wrong")
}

main(args:int[][]) {
  a:int = 1
  println(unparseInt(a))

  testBooleans(a)
}