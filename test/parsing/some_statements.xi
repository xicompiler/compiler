fun_fun(): int[], int {
  some_variable'name'1234:int[] = "Helloooo"
  some_variable'name'1234 = "World" x:int = 3
  return some_variable'name'1234, 3
}

main(args:int[][]) {
  _, a':int, b:int = fun_fun();
  c:int = a' *>> 5;
  d:int[] = "World" + {2, 3}
  return length(d) != b
}