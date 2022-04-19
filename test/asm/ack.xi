use io
use conv

usage() {
    println("Please specify the input size")
}

main(args:int[][]) {
    println("main")

    n: int = 11

    println("n assigned")

    x: int = foo()
    println(unparseInt(x))
    r: int = ack(2, n)

    println("r assigned")
    
    print("Ack(2,")
    print(unparseInt(n))
    print("): ")
    print(unparseInt(r))
    println("")

    println("done")
}

foo() : int {
    return 1
}

ack(m:int, n:int):int {
    if (m == 0) { return n+1 }
    else if (n == 0) { return ack(m-1, 1) }
    else { return ack(m-1, ack(m, n-1)) }
}
