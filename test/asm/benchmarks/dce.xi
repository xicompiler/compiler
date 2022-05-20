use io
use conv

f(n:int) : int {
    return 0;
}

main(args:int[][]) {
    a:int = 1
    b:int = f(2)
    c:int = f(f(3))
    d:int = f(f(f(4)))
    e:int = f(f(f(f(5))))
    f:int = f(f(f(f(f(6)))))
    i:int = f(f(f(f(f(f(7))))))
    j:int = f(f(f(f(f(f(f(8)))))))
    k:int = f(f(f(f(f(f(f(f(9))))))))
    l:int = f(f(f(f(f(f(f(f(f(10)))))))))
    print(unparseInt(a))
}