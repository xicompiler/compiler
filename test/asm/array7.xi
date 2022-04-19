use io
use conv

main(args:int[][]) {
    a : int[] = {0}
    while a[0] < 10 {
        a[0] = a[0] + 1
        println(unparseInt(a[0]))
    }
}