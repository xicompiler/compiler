foo(x : int) : bool {
    return bar(false) > 0
}

bar(x : bool) : int {
    if (foo(1)) {
        return 1;
    }
    return 0;
}