use io
use conv

main(args:int[][]) {
    arr:int[][], arr2:int[][], arr3:int[][] = make_array(1,2,3,4)
    println(unparseInt(arr[0][0]))
    println(unparseInt(arr[0][1]))
    println(unparseInt(arr[1][0]))
    println(unparseInt(arr[1][1]))
    println(unparseInt(arr[2][0]))
    println(unparseInt(arr[2][1]))
    println(unparseInt(arr[3][0]))
    println(unparseInt(arr[3][1]))
    println(unparseInt(arr2[0][0]))
    println(unparseInt(arr2[0][1]))
    println(unparseInt(arr2[1][0]))
    println(unparseInt(arr2[1][1]))
    println(unparseInt(arr2[2][0]))
    println(unparseInt(arr2[2][1]))
    println(unparseInt(arr2[3][0]))
    println(unparseInt(arr2[3][1]))
    println(unparseInt(arr3[0][0]))
    println(unparseInt(arr3[0][1]))
    println(unparseInt(arr3[0][2]))
    println(unparseInt(arr3[0][3]))
    println(unparseInt(arr3[1][0]))
    println(unparseInt(arr3[1][1]))
    println(unparseInt(arr3[1][2]))
    println(unparseInt(arr3[1][3]))
    println(unparseInt(arr3[2][0]))
    println(unparseInt(arr3[2][1]))
    println(unparseInt(arr3[2][2]))
    println(unparseInt(arr3[2][3]))
    println(unparseInt(arr3[3][0]))
    println(unparseInt(arr3[3][1]))
    println(unparseInt(arr3[3][2]))
    println(unparseInt(arr3[3][3]))
    
}

make_array(a:int, b:int, c:int, d:int) :int[][], int[][], int[][] {
    arr : int[][] = {{a, a}, {b, b}, {c, c}, {d,d}}
    arr_rev : int[][] = {{d,d}, {c,c}, {b,b}, {a,a}}
    arr2 : int[][] = {{a, b, c, d},{d, c, b, a}, {a, b, c, d},{d, c, b, a}}
    return arr, arr_rev, arr2
}