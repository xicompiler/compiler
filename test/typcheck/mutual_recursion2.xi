foo(x : int) : bool {
    return bar(1) > 0
}

bar(x : int) : int {
    if (foo(1)) {
        return true;
    }
    return false;
}