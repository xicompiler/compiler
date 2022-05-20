use io
use conv

f(n:int, n2:int, n3:int) : int {
    return n+n2+n3;
}

main(args:int[][]) {
    x:int = 1
    y:int = 1
    z:int = 1
    a:int = f(x, y, z)
    println(unparseInt(a))
    b:int = 10
    c:int = 12
    d:int = 20
    e:int = f(b, c, d)
    print(unparseInt(e))
}