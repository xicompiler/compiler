use io 
use conv

global:int = 5

main(args:int[][]) {
    print(unparseInt(global))
    global = global + 1
    foo()
}

foo() {
    print(unparseInt(global))
    global = global + 1
    bar()
}

bar() {
    print(unparseInt(global))
}

