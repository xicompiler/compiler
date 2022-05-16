use io
use conv

f(n:int) : int {
    return 0;
}

main(args:int[][]) {
    a:int = f(2)
    a:int = f(f(3))
    a:int = f(f(f(4)))
    a:int = f(f(f(f(5))))
    a:int = f(f(f(f(f(6)))))
    a:int = f(f(f(f(f(f(7))))))
    a:int = f(f(f(f(f(f(f(8)))))))
    a:int = f(f(f(f(f(f(f(f(9))))))))
    a:int = f(f(f(f(f(f(f(f(f(10)))))))))
    a:int = 1
    print(unparseInt(a))
}