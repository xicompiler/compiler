use io
use conv

main(args:int[][]) {
    n:int, 
    m:int, 
    o:int, 
    p:int, 
    q:int, 
    r:int, 
    s:int, 
    t:int = foo(32, 34, 13, 14, 15, 16, 17, 38)
    println("n " + unparseInt(n))
    println("m " + unparseInt(m))
    println("o " + unparseInt(o))
    println("p " + unparseInt(p))
    println("q " + unparseInt(q))
    println("r " + unparseInt(r))
    println("s " + unparseInt(s))
    println("t " + unparseInt(t))
}

foo(a:int, b:int, c:int, d:int, e:int, f:int, g:int, h:int) 
: int, int, int, int, int, int, int, int {
    println("a " + unparseInt(a))
    println("b " + unparseInt(b))
    println("c " + unparseInt(c))
    println("d " + unparseInt(d))
    println("e " + unparseInt(e))
    println("f " + unparseInt(f))
    println("g " + unparseInt(g))
    println("h " + unparseInt(h))
    return 22, 24, 3, 4, 5, 6, 7, 28
}
