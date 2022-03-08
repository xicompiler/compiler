main() : bool {
  return {foo()} == {1, 2}
}

foo() : int, int {
  return 1, 2
}