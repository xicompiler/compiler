use io
use conv

id(x : int) : int {
    println(unparseInt(x))
    return x
}

main(args: int[][]) {
    a : int[3]
    i : int = 0

    while (i < length(a)) {
        a[i] = i + 1
        i = i + 1
    }

    i = 0
    println(unparseInt(length(a)))

    while (i < length(a)) {
        println(unparseInt(a[id(i)]))
        i = i + 1
    }
}