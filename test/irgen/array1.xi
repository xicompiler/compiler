use io
use conv

id(x: int): int {
    println(unparseInt(x))
    return x
}

main(args: int[][]) {
    a: int[3]
    a[id(0)] = 1
    a[id(1)] = 2
    a[id(2)] = 3
    println(unparseInt(length(a)))
    println(unparseInt(a[0]))
    println(unparseInt(a[1]))
    println(unparseInt(a[2]))
}