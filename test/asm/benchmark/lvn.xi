use io
use conv

main(args:int[][]) {
    a:int = 1
    b:int = a + 1
    c:int = 1 + a
    println(unparseInt(c))
    d:int = 2
    e:int = d + 2
    f:int = 2 + d
    print(unparseInt(c))
}