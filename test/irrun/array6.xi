use io
use conv

foo(a: int[], i: int): int {
    println(unparseInt(i))
    a[i] = a[i] + 1
    return a[i]
}

main(args: int[][]) {
    a: int[] = {1, 2, 3, 4}
    arr_decl: int[foo(a, 1)][foo(a, 2) - foo(a, 0)][foo(a, 3)][][]

    i : int = 0
    while (i < length(a)) {
        println(unparseInt(a[i]))
        i = i + 1
    }
}