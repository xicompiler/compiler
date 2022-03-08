a() : int, int {
    return 1, 2
}

b() : int, int {
    return 1, 2
}

main() {
    result : bool = a() == b();
}