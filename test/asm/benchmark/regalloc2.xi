use conv
use io

foo(a:int, b:int, c:int, d:int, e:int, f:int, g:int, h:int) {
    x:int = a+b
    y:int = x+a+b
    z:int = c+d*2
    p:int = h+z+y+x+g
    q:int = p+c+g+f+d
    r:int = x+y+z+a+b+c+d+e+f
    t:int = h+g*2+7+p+q
    k:int = a+b+c+d+e+f+g+h+x+y+z+p+q+r+t-a-b-c-d-e-f-g-h-x-y-z-p-q-r-t
    print(unparseInt(k))
}

main(args:int[][]) {
    foo(1, 2, 3, 4, 5, 6, 7, 8)
}