use io

main(args:int[][]) {
    println("main")

    a:int[] = {1, 2, 3}
    b:int[] = {4, 5, 6}

    c:int[] = a + b

    if (c[0] == 1 & c[3] == 4) {
      println("correct")
    }
    else {
      println ("wrong")
    }

    println("concatenating " + "strings")
}