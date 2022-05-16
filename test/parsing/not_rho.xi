use io
use conv

main(args:int[][]) {
    a:int = 2
    b:int = 3
    while(a != 0) {
        a = a - 1
          while(b > 0) {
            b = b - 1
            if(b == 1) {
              break
            }
          }
          break
    }
    println(unparseInt(a))
    println(unparseInt(b))
}