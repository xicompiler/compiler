use io
use conv

main(args:int[][]) {
    a:int[], b:int[], c:int[], d:int[], e:int[],f:int[], g:int[], h:int[] = multi_id(1,2,3,4,5,6,7,8)
    println(unparseInt(a[0]))
    println(unparseInt(b[0]))
    println(unparseInt(c[0]))
    println(unparseInt(d[0]))
    println(unparseInt(e[0]))
    println(unparseInt(f[0]))
    println(unparseInt(g[0]))
    println(unparseInt(h[0]))
}

multi_id(a:int, b:int, c:int, d:int, e:int,f:int, g:int, h:int) : int[], int[], int[], int[], int[], int[], int[], int[]{
    arr:int[] = {a}
    barr:int[] = {b}
    carr:int[] = {c}
    darr:int[] = {d}
    earr:int[] = {e}
    farr:int[] = {f}
    garr:int[] = {g}
    harr:int[] = {h}
    return arr, barr, carr, darr, earr, farr, garr, harr
}