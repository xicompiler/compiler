use io
use conv

main(args:int[][]) {
    x:int = 1
    y:int = 1
    z:int = 1
    a:int = x+y+z
    println(unparseInt(a))
    b:int = 1
    c:int = 1
    d:int = 1
    e:int = b+c+d
    print(unparseInt(e))
}