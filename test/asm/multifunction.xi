use io
use conv

main(args:int[][]) {
    a:int, b:int, c:int, d:int, e:int,f:int, g:int, h:int = multi_id(1,2,3,4,5,6,7,8)
    println(unparseInt(a))
    println(unparseInt(b))
    println(unparseInt(c))
    println(unparseInt(d))
    println(unparseInt(e))
    println(unparseInt(f))
    println(unparseInt(g))
    println(unparseInt(h))
}

multi_id(a:int, b:int, c:int, d:int, e:int,f:int, g:int, h:int) : int, int, int, int, int, int, int, int{
    return a, b, c, d, e, f, g, h
}