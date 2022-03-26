use io
use conv

booleanstring(b : bool) : int[]{
    if (b) {
        return unparseInt(1)
    }
    else {
        return unparseInt(0)
    }
}

main(args:int[][]) {
    println(unparseInt(1+1))
    println(unparseInt(1-1))
    println(unparseInt(1*1))
    println(unparseInt(1/1))
    println(unparseInt(1%1))
    println(unparseInt(-1))
    println(unparseInt(1 *>> 1))
    println(booleanstring(1==1))
    println(booleanstring(1==2))
    println(booleanstring(1!=1))
    println(booleanstring(1!=2))
    println(booleanstring(1<1))
    println(booleanstring(1<2))
    println(booleanstring(1>1))
    println(booleanstring(2>1))
    println(booleanstring(1<=1))
    println(booleanstring(1<=0))
    println(booleanstring(1>=1))
    println(booleanstring(0>=1))
    println(booleanstring(!!true))
    println(booleanstring(!!!false))
    println(booleanstring(!!!!false))
}
