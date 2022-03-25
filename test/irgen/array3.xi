use io
use conv

id(x : int) : int {
    println(unparseInt(x))
    return x
}

main(args: int[][]) {
    a : int[3][4]
    i : int = 0

    while (i < length(a)) {
        j : int = 0
        while (j < length(a[0])) {
            a[i][j] = i + j
            j = j + 1
        }
        i = i + 1
    }

    i = 0
    println(unparseInt(length(a)))

    while (i < length(a)) {
        j : int = 0
        while (j < length(a[0])) {
            println(unparseInt(a[id(i)][id(j)]))
            j = j + 1
        }
        i = i + 1
    }
}