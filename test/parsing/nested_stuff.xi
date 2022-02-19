// valid global variable declarations
x:int = 2;
z:int
len: int = 100
debug: bool = false
points: int[]

nesteoutuff(a:int, b:int, c:int):int {
  asdf:bool = true
  while(a != 0) {
    while(b != 0) {
      if(c == 1) asdf = false
      else asdf = !asdf
      if(a < b) {
        if(a == b - 1) {
          return 1
        }
      }
      b = b - 1;
    }
    a = a - 1
  }
  return 2
}


