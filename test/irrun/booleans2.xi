use io
use conv

booleanstring(b : bool) : int[] {
    if (b) {
        return unparseInt(1)
    }
    else {
        return unparseInt(0)
    }
}

main(args:int[][]) {
    println(booleanstring(true == true))
    println(booleanstring(true == false))
    println(booleanstring(false == true))
    println(booleanstring(false == false))
    println(booleanstring(true != true))
    println(booleanstring(true != false))
    println(booleanstring(false != true))
    println(booleanstring(false != false))
    println(booleanstring(true & true))
    println(booleanstring(true & false))
    println(booleanstring(false & true))
    println(booleanstring(false & false))
    println(booleanstring(true | true))
    println(booleanstring(true | false))
    println(booleanstring(false | true))
    println(booleanstring(false | false))
    println(booleanstring(!true))
    println(booleanstring(!false))
}