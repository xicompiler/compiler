use io
use conv

main(args:int[][]) {
    x:int = 1
    y:int = x
    z:int = y + 1
    println(unparseInt(z))
    a:int = z
    b:int = a + 1
    println(unparseInt(b))
    c:int = b
    d:int = c + 1
    println(unparseInt(d))
    e:int = d
    f:int = e + 1
    print(unparseInt(f))
}