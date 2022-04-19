use io
use conv

main(args:int[][]) {
    arr : int[] = {1,2,3,4,5,6,7,8}
    a:int, b:int, c:int, d:int, e:int,f:int, g:int, h:int = sep_array(arr)
    println(unparseInt(a))
    println(unparseInt(b))
    println(unparseInt(c))
    println(unparseInt(d))
    println(unparseInt(e))
    println(unparseInt(f))
    println(unparseInt(g))
    println(unparseInt(h))
}

sep_array(a:int[]) : int, int, int, int, int, int, int, int{
    return a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7]
}