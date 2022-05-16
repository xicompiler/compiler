use io
use conv

main(args:int[][]) {
    a:int = 1
    b:int = a+a
    println(unparseInt(b))
    c:int = a+a+b
    println(unparseInt(c))
    d:int = a+a+b+c
    println(unparseInt(d))
    e:int = a+a+b+c+d
    println(unparseInt(e))
    f:int = a+a+b+c+d+e
    println(unparseInt(f))
    g:int = a+a+b+c+d+e+f
    print(unparseInt(g))
    z:int = a+a
    println(unparseInt(z))
}