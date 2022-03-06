foo() : bool {
    return bar() > 0
}

bar() : int {
    if (foo()) {
        return true;
    }
    return false;
}