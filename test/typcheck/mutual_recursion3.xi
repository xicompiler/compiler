a(b : int) : bool {
    return b(false) > 0
}

b(a : bool) : int {
    if (a(1)) {
        return 1;
    }
    return 0;
}