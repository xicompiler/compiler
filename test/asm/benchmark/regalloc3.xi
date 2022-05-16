use io
use conv

main(args:int[][]) {
    a:int = 10;
    b:int = a+a;
    c:int = b+b;
    d:int = c+c;
    e:int = d+d;
    f:int = e+e;
    g:int = d+d;
    h:int = c+c;
    i:int = b+b;
    j:int = a+a;
    k:int = a+b+c+d+e+f+g+h+i+j;
    print(unparseInt(k))
}