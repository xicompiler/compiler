use io
use conv

f(n:int, n2:int, n3:int) : int {
    return n+n2+n3;
}

main(args:int[][]) {
    x:int = 1
    y:int = x
    z:int = x
    a:int = x
    b:int = f(y, z, a)
    println(unparseInt(b))
    c:int = b
    d:int = b
    e:int = b
    f:int = f(c, d, e)
    print(unparseInt(f))
}