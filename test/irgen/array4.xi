use io
use conv

id(x : int) : int {
    println(unparseInt(x))
    return x
}

ft(a : int[], x : int): int {
    println(unparseInt(x))
    a[x] = 42
    return x
}

main(args : int[][]) {
    a : int[] = {1, 2, 3}

    println(unparseInt(length(a)))
    println(unparseInt(a[0]))
    println(unparseInt(a[1]))
    println(unparseInt(a[2]))

    a[id(a[id(0)])] = a[ft(a, 2)]

    println(unparseInt(a[0]))
    println(unparseInt(a[1]))
    println(unparseInt(a[2]))
}