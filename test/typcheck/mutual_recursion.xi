foo() : bool {
  return bar() > 0
}

bar() : int {
  if (foo()) {
    return -1;
  }
  return 1;
}