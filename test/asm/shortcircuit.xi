use io
use conv

compare(x : int, y : int) : bool{
    println("compare")
    return x < y
}

main(args:int[][]) {
    if (compare(1, 2) | compare(1, 2)) {
        println("or compare")
    }
    else {
        println("wrong or")
    }
    if (compare(2, 2) & compare(1, 2)) {
        println("wrong and")
    }
    else {
        println("and compare")
    }
}