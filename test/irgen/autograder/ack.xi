use io
use conv

usage() {
    println("Please specify the input size")
}

main(args:int[][]) {
    println("main")

    n: int = 11

    println("n assigned")

    r: int = Ack(2, n)

    println("r assigned")
    
    print("Ack(2,")
    print(unparseInt(n))
    print("): ")
    print(unparseInt(r))
    println("")

    println("done")
}

Ack(m:int, n:int):int {
    print("ack")
    if (m == 0) { return n+1 }
    else if (n == 0) { return Ack(m-1, 1) }
    else { return Ack(m-1, Ack(m, n-1)) }
}
