use io
use conv

main(args:int[][]) {
    a:int, b:int, c:int, d:int, e:int, f:int = call(1,2,3,4,5,6)
    println(unparseInt(a))
    println(unparseInt(b))
    println(unparseInt(c))
    println(unparseInt(d))
    println(unparseInt(e))
    println(unparseInt(f))
}

call(a:int, b:int, c:int, d:int, e:int, f:int) : int, int, int, int, int, int {
    aa:int, bb:int, cc:int, dd:int, ee:int, ff:int = call2(a,b,c,d,e,f)
    return aa,bb,cc,dd,ee,ff
}

call2(a:int, b:int, c:int, d:int, e:int, f:int) : int, int, int, int, int, int {
    aa:int, bb:int, cc:int, dd:int, ee:int, ff:int = call4(a,b,c,d,e,f)
    return aa,bb,cc,dd,ee,ff
}

call4(a:int, b:int, c:int, d:int, e:int, f:int) : int, int, int, int, int, int {
    aa:int, bb:int, cc:int, dd:int, ee:int, ff:int = call5(a,b,c,d,e,f)
    return aa,bb,cc,dd,ee,ff
}

call5(a:int, b:int, c:int, d:int, e:int, f:int) : int, int, int, int, int, int {
    return a,b,c,d,e,f
}