use io

array_declarations(m:int, n:int):int[][] {
    a: int[][]
    b: int[m][n]
    c: int[3][]
    c[0] = b[0]; c[1] = b[1]; c[2] = b[2]
    d: int[][] = {{1, 0}, {0, 1}}
    return d
}

array_access(): int[], int {
    a: int[][] = array_declarations(3, 4)
    b: int[] = a[0]
    c: int = a[1][1]
    return b, c
}

main(args:int[][]) {
    println("main")

    print("a")
    a: int[][]
    print("b")
    b: int[4][5]
    print("c")
    c: int[3][]
    c[0] = b[0]; c[1] = b[1]; c[2] = b[2]
    println("d")
    d: int[][] = {{1, 0}, {0, 1}}

    if (d[1][1] == 1) {
      println("correct")
    }
    else {
      println ("wrong")
    }

    println("done")
}