use io 
use conv

global:int = 5

main(args:int[][]) {
    global = global + 1
    foo()
}

foo() {
    print(unparseInt(global))
}