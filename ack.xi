use io
use conv

// usage() {
    //     println("Please specify the input size")
// }


bar(x : int, y : int) {
    println("bar")
}

main(args:int[][]) {
    println("main")
    n: int = foo()
    r: int = ack(100000, 0)
    
    println(unparseInt(r))
    bar(1, 2)
}

foo() : int {
    return 1
}

acker(m:int, n:int):int {
    return m + n
}

ack(m:int, n:int):int {
    if (m == 0) {  println("returning") return n+1 }
    else if (n == 0) { return ack(m-1, 1) }
    else { return ack(m-1, ack(m, n-1)) }
}
